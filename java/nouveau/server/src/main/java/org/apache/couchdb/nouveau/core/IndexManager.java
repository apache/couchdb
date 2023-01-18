//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.apache.couchdb.nouveau.core;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Stream;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.misc.store.DirectIODirectory;
import org.apache.lucene.search.SearcherFactory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.LockObtainFailedException;
import org.apache.lucene.util.IOUtils;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.github.benmanes.caffeine.cache.Scheduler;

import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.caffeine.MetricsStatsCounter;

import io.dropwizard.lifecycle.Managed;

public class IndexManager implements Managed {

    private static final int RETRY_LIMIT = 500;
    private static final int RETRY_SLEEP_MS = 5;
    private static final Logger LOGGER = LoggerFactory.getLogger(IndexManager.class);

    public class Index {
        private static final String DEFAULT_FIELD = "default";
        private final String name;
        private IndexWriter writer;
        private SearcherManager searcherManager;
        private Analyzer analyzer;
        private final AtomicBoolean deleteOnClose = new AtomicBoolean();
        private final AtomicLong updateSeq = new AtomicLong();

        // The write lock is to ensure there are no readers/searchers when
        // we want to close the index.
        private ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
        private Lock rl = rwl.readLock();
        private Lock wl = rwl.writeLock();

        private Index(
                      String name,
                      IndexWriter writer,
                      SearcherManager searcherManager,
                      Analyzer analyzer,
                      long updateSeq) {
            this.name = name;
            this.writer = writer;
            this.searcherManager = searcherManager;
            this.analyzer = analyzer;
            this.updateSeq.set(updateSeq);
        }

        public String getName() {
            return name;
        }

        public IndexWriter getWriter() {
            return writer;
        }

        public SearcherManager getSearcherManager() {
            return searcherManager;
        }

        public QueryParser getQueryParser() {
            return new NouveauQueryParser(DEFAULT_FIELD, analyzer);
        }

        public boolean commit() throws IOException {
            rl.lock();
            try {
                writer.setLiveCommitData(generateCommitData().entrySet());
                return writer.commit() != -1;
            } finally {
                rl.unlock();
            }
        }

        public long getUpdateSeq() throws IOException {
            return updateSeq.get();
        }

        public void incrementUpdateSeq(final long updateSeq) throws IOException {
            final long newSeq = this.updateSeq.accumulateAndGet(updateSeq, (a, b) -> Math.max(a, b));
            if (newSeq != updateSeq) {
                throw new UpdatesOutOfOrderException();
            }
        }

        public void close() throws IOException {
            wl.lock();
            try {
                if (writer == null) {
                    // Already closed.
                    return;
                }

                // Close searcher manager
                if (searcherManager != null) {
                    try {
                        searcherManager.close();
                    } catch (IOException e) {
                        LOGGER.info(this + " threw exception when closing searcherManager.", e);
                    } finally {
                        searcherManager = null;
                    }
                }

                if (deleteOnClose.get()) {
                    try {
                        // No need to commit in this case.
                        writer.rollback();
                    } catch (IOException e) {
                        LOGGER.info(this + " threw exception when rolling back writer.", e);
                    } finally {
                        writer = null;
                    }
                    IOUtils.rm(indexRootPath(name));
                } else {
                    try {
                        writer.setLiveCommitData(generateCommitData().entrySet());
                        writer.close();
                        LOGGER.info("{} closed.", this);
                    } finally {
                        writer = null;
                    }
                }
            } finally {
                wl.unlock();
            }
        }

        private Map<String, String> generateCommitData() {
            return Collections.singletonMap("update_seq", Long.toString(updateSeq.get()));
        }

        @Override
        public String toString() {
            return "Index [name=" + name + "]";
        }
    }

    private class IndexLoader implements CacheLoader<String, Index> {

        @Override
        public @Nullable Index load(@NonNull String name) throws Exception {
            return openExistingIndex(name);
        }

        @Override
        public @Nullable Index reload(@NonNull String name, @NonNull Index index) throws Exception {
            try {
                if (index.commit()) {
                    LOGGER.info("{} committed.", index);
                }
            } catch (final IOException e) {
                LOGGER.error(index + " threw exception when committing.", e);
                index.close();
                return openExistingIndex(name);
            }
            return index;
        }

    }

    private static class IndexCloser implements RemovalListener<String, Index> {

        public void onRemoval(String name, Index index, RemovalCause cause) {
            try {
                index.close();
            } catch (IOException e) {
                LOGGER.error(index + " threw exception when closing", e);
            }
        }
    }

    private static final IndexCloser INDEX_CLOSER = new IndexCloser();


    @Min(1)
    private int maxIndexesOpen;

    @Min(1)
    private int commitIntervalSeconds;

    @Min(1)
    private int idleSeconds;

    @NotEmpty
    private Path rootDir;

    @NotNull
    private AnalyzerFactory analyzerFactory;

    @NotNull
    private ObjectMapper objectMapper;

    private SearcherFactory searcherFactory;

    private MetricRegistry metricRegistry;

    private LoadingCache<String, Index> cache;

    public Index acquire(final String name) throws IOException {
        for (int i = 0; i < RETRY_LIMIT; i++) {
            final Index result = getFromCache(name);

            // Check if we're in the middle of closing.
            result.rl.lock();
            if (result.writer != null) {
                return result;
            }
            result.rl.unlock();

            // Retry after a short sleep.
            try {
                Thread.sleep(RETRY_SLEEP_MS);
            } catch (InterruptedException e) {
                Thread.interrupted();
                break;
            }
        }
        throw new IOException("Failed to acquire " + name);
    }

    public void release(final Index index) throws IOException {
        index.rl.unlock();
    }

    public void create(final String name, IndexDefinition indexDefinition) throws IOException {
        createNewIndex(name, indexDefinition);
    }

    public void deleteAll(final String path) throws IOException {
        final Path rootPath = indexRootPath(path);
        if (!rootPath.toFile().exists()) {
            return;
        }
        Stream<Path> stream = Files.find(rootPath, 100,
            (p, attr) -> attr.isDirectory() && isIndex(p));
        try {
            stream.forEach((p) -> {
                try {
                    deleteIndex(rootDir.relativize(p).toString());
                } catch (Exception e) {
                    LOGGER.error("I/O exception deleting " + p, e);
                }
            });
        } finally {
            stream.close();
        }
    }

    private void deleteIndex(final String name) throws IOException {
        final Index index = acquire(name);
        try {
            index.deleteOnClose.set(true);
            cache.invalidate(name);
        } finally {
            release(index);
        }
    }

    @JsonProperty
    public int getMaxIndexesOpen() {
        return maxIndexesOpen;
    }

    public void setMaxIndexesOpen(int maxIndexesOpen) {
        this.maxIndexesOpen = maxIndexesOpen;
    }

    @JsonProperty
    public int getCommitIntervalSeconds() {
        return commitIntervalSeconds;
    }

    public void setCommitIntervalSeconds(int commitIntervalSeconds) {
        this.commitIntervalSeconds = commitIntervalSeconds;
    }

    @JsonProperty
    public int getIdleSeconds() {
        return idleSeconds;
    }

    public void setIdleSeconds(int idleSeconds) {
        this.idleSeconds = idleSeconds;
    }

    @JsonProperty
    public Path getRootDir() {
        return rootDir;
    }

    public void setRootDir(Path rootDir) {
        this.rootDir = rootDir;
    }

    public void setAnalyzerFactory(final AnalyzerFactory analyzerFactory) {
        this.analyzerFactory = analyzerFactory;
    }

    public void setObjectMapper(final ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public void setSearcherFactory(final SearcherFactory searcherFactory) {
        this.searcherFactory = searcherFactory;
    }

    public void setMetricRegistry(final MetricRegistry metricRegistry) {
        this.metricRegistry = metricRegistry;
    }

    @Override
    public void start() throws IOException {
        cache = Caffeine.newBuilder()
            .recordStats(() -> new MetricsStatsCounter(metricRegistry, "IndexManager"))
            .initialCapacity(maxIndexesOpen)
            .maximumSize(maxIndexesOpen)
            .expireAfterAccess(Duration.ofSeconds(idleSeconds))
            .expireAfterWrite(Duration.ofSeconds(idleSeconds))
            .refreshAfterWrite(Duration.ofSeconds(commitIntervalSeconds))
            .scheduler(Scheduler.systemScheduler())
            .removalListener(INDEX_CLOSER)
            .evictionListener(INDEX_CLOSER)
            .build(new IndexLoader());
    }

    @Override
    public void stop() {
        cache.invalidateAll();
    }

    private Index getFromCache(final String name) throws IOException {
        try {
            return cache.get(name);
        } catch (CompletionException e) {
            if (e.getCause() instanceof IOException) {
                throw (IOException) e.getCause();
            }
            throw e;
        }
    }

    private void createNewIndex(final String name, final IndexDefinition indexDefinition) throws IOException {
        // Validate index definiton
        analyzerFactory.fromDefinition(indexDefinition);

        // Persist definition
        final Path path = indexDefinitionPath(name);
        if (Files.exists(path)) {
            throw new FileAlreadyExistsException(name + " already exists");
        }
        Files.createDirectories(path.getParent());
        objectMapper.writeValue(path.toFile(), indexDefinition);
    }

    private Index openExistingIndex(final String name) throws IOException {
        final IndexDefinition indexDefinition = objectMapper.readValue(indexDefinitionPath(name).toFile(), IndexDefinition.class);
        final Analyzer analyzer =  analyzerFactory.fromDefinition(indexDefinition);
        final Path path = indexPath(name);
        final Directory dir = directory(path);
        final IndexWriter writer = newWriter(dir, analyzer);
        final SearcherManager searcherManager = new SearcherManager(writer, searcherFactory);
        final long updateSeq = getUpdateSeq(writer);
        return new Index(name, writer, searcherManager, analyzer, updateSeq);
    }

    private long getUpdateSeq(final IndexWriter writer) throws IOException {
        final Iterable<Map.Entry<String, String>> commitData = writer.getLiveCommitData();
        if (commitData == null) {
            return 0L;
        }
        for (Map.Entry<String, String> entry : commitData) {
            if (entry.getKey().equals("update_seq")) {
                return Long.parseLong(entry.getValue());
            }
        }
        return 0L;
    }

    private IndexWriter newWriter(final Directory dir, final Analyzer analyzer) throws IOException {
        LockObtainFailedException exceptionThrown = null;
        for (int i = 0; i < RETRY_LIMIT; i++) {
            try {
                final IndexWriterConfig config = new IndexWriterConfig(analyzer);
                config.setCommitOnClose(true);
                config.setUseCompoundFile(false);
                return new IndexWriter(dir, config);
            } catch (LockObtainFailedException e) {
                exceptionThrown = e;
                try {
                    Thread.sleep(RETRY_SLEEP_MS);
                } catch (InterruptedException e1) {
                    Thread.interrupted();
                    break;
                }
            }
        }
        throw exceptionThrown;
    }

    private boolean isIndex(final Path path) {
        return path.resolve("index_definition.json").toFile().exists();
    }

    private Path indexDefinitionPath(final String name) {
        return indexRootPath(name).resolve("index_definition.json");
    }

    private Path indexPath(final String name) {
        return indexRootPath(name).resolve("index");
    }

    private Path indexRootPath(final String name) {
        final Path result = rootDir.resolve(name).normalize();
        if (result.startsWith(rootDir)) {
            return result;
        }
        throw new WebApplicationException(name + " attempts to escape from index root directory",
                Status.BAD_REQUEST);
    }

    private Directory directory(final Path path) throws IOException {
        return new DirectIODirectory(FSDirectory.open(path));
    }

}
