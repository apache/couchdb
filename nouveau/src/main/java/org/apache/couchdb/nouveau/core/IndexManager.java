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

import static com.codahale.metrics.MetricRegistry.name;

import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.caffeine.MetricsStatsCounter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.AsyncCacheLoader;
import com.github.benmanes.caffeine.cache.AsyncLoadingCache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.github.benmanes.caffeine.cache.Scheduler;
import com.github.benmanes.caffeine.cache.Weigher;
import io.dropwizard.lifecycle.Managed;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response.Status;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.locks.Lock;
import java.util.stream.Stream;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.lucene9.Lucene9AnalyzerFactory;
import org.apache.couchdb.nouveau.lucene9.Lucene9Index;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.misc.store.DirectIODirectory;
import org.apache.lucene.search.SearcherFactory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.checkerframework.checker.index.qual.NonNegative;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The central class of Nouveau, responsible for loading and unloading Lucene
 * indexes and making them available for query.
 */
public final class IndexManager implements Managed {

    @FunctionalInterface
    public interface IndexFunction<V, R> {
        R apply(final V value) throws IOException;
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(IndexManager.class);

    private int maxIndexesOpen;

    private int commitIntervalSeconds;

    private int idleSeconds;

    private Path rootDir;

    private ObjectMapper objectMapper;

    private MetricRegistry metricRegistry;

    private Scheduler scheduler;

    private SearcherFactory searcherFactory;

    private AsyncLoadingCache<String, Index> cache;

    private StripedLock<String> lock;

    public <R> R with(final String name, final IndexFunction<Index, R> indexFun)
            throws IOException, InterruptedException {
        while (true) {
            if (!exists(name)) {
                throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
            }

            final CompletableFuture<Index> future = cache.get(name);
            final Index index = future.join();

            if (index.tryAcquire()) {
                try {
                    return indexFun.apply(index);
                } finally {
                    index.release();
                }
            }
        }
    }

    public void create(final String name, IndexDefinition indexDefinition) throws IOException {
        if (exists(name)) {
            assertSame(indexDefinition, loadIndexDefinition(name));
            return;
        }

        final Lock lock = this.lock.writeLock(name);
        lock.lock();
        try {
            if (exists(name)) {
                assertSame(indexDefinition, loadIndexDefinition(name));
                return;
            }
            final Path dstFile = indexDefinitionPath(name);
            Files.createDirectories(dstFile.getParent());
            final Path tmpFile = Files.createTempFile(dstFile.getParent(), null, null);
            boolean success = false;
            try {
                objectMapper.writeValue(tmpFile.toFile(), indexDefinition);
                Files.move(tmpFile, dstFile, StandardCopyOption.ATOMIC_MOVE);
                success = true;
            } finally {
                if (!success) {
                    Files.delete(tmpFile);
                }
            }
        } finally {
            lock.unlock();
        }
    }

    public boolean exists(final String name) {
        return Files.exists(indexDefinitionPath(name));
    }

    public void deleteAll(final String path, final List<String> exclusions) throws IOException {
        LOGGER.info("deleting indexes below {} (excluding {})", path, exclusions == null ? "nothing" : exclusions);

        final Path indexRootPath = indexRootPath(path);
        if (!indexRootPath.toFile().exists()) {
            return;
        }
        Stream<Path> stream = Files.find(indexRootPath, 100, (p, attr) -> attr.isDirectory() && isIndex(p));
        try {
            stream.forEach((p) -> {
                final String relativeToExclusions = indexRootPath.relativize(p).toString();
                if (exclusions != null && exclusions.indexOf(relativeToExclusions) != -1) {
                    return;
                }
                final String relativeName = rootDir.relativize(p).toString();
                try {
                    deleteIndex(relativeName);
                } catch (final IOException e) {
                    LOGGER.error("I/O exception deleting " + p, e);
                }
                // Clean any newly empty directories.
                do {
                    final File f = p.toFile();
                    if (f.isDirectory() && f.list().length == 0) {
                        f.delete();
                    }
                } while ((p = p.getParent()) != null && !rootDir.equals(p));
            });
        } finally {
            stream.close();
        }
    }

    private void deleteIndex(final String name) throws IOException {
        final CompletableFuture<Index> future = cache.asMap().remove(name);
        if (future == null) {
            return;
        }
        final Index index = future.getNow(null);
        if (index != null) {
            index.setDeleteOnClose(true);
            close(name, index);
        } else {
            IOUtils.rm(indexRootPath(name));
        }
    }

    public void setMaxIndexesOpen(int maxIndexesOpen) {
        this.maxIndexesOpen = maxIndexesOpen;
    }

    public void setCommitIntervalSeconds(int commitIntervalSeconds) {
        this.commitIntervalSeconds = commitIntervalSeconds;
    }

    public void setIdleSeconds(int idleSeconds) {
        this.idleSeconds = idleSeconds;
    }

    public void setRootDir(Path rootDir) {
        this.rootDir = rootDir;
    }

    public void setObjectMapper(final ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public void setMetricRegistry(final MetricRegistry metricRegistry) {
        this.metricRegistry = metricRegistry;
    }

    public void setScheduler(final Scheduler scheduler) {
        this.scheduler = scheduler;
    }

    public void setSearcherFactory(final SearcherFactory searcherFactory) {
        this.searcherFactory = searcherFactory;
    }

    @Override
    public void start() throws IOException {
        cache = Caffeine.newBuilder()
                .recordStats(() -> new MetricsStatsCounter(metricRegistry, name(IndexManager.class, "cache")))
                .initialCapacity(maxIndexesOpen)
                .maximumWeight(maxIndexesOpen)
                .weigher(new IndexWeigher())
                .expireAfterAccess(Duration.ofSeconds(idleSeconds))
                .refreshAfterWrite(Duration.ofSeconds(commitIntervalSeconds))
                .scheduler(scheduler)
                .evictionListener(new IndexEvictionListener())
                .buildAsync(new AsyncIndexLoader());
        lock = new StripedLock<String>(100);
    }

    @Override
    public void stop() throws IOException, InterruptedException {
        final var it = cache.asMap().entrySet().iterator();
        while (it.hasNext()) {
            var e = it.next();
            LOGGER.info("closing {} during shutdown", e.getKey());
            close(e.getKey(), e.getValue());
            it.remove();
        }
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

    private IndexDefinition loadIndexDefinition(final String name) throws IOException {
        return objectMapper.readValue(indexDefinitionPath(name).toFile(), IndexDefinition.class);
    }

    private Path indexRootPath(final String name) {
        final Path result = rootDir.resolve(name);
        if (result.startsWith(rootDir)) {
            return result;
        }
        throw new WebApplicationException(name + " attempts to escape from index root directory", Status.BAD_REQUEST);
    }

    private class IndexEvictionListener implements RemovalListener<String, Index> {

        public void onRemoval(String name, Index index, RemovalCause cause) {
            LOGGER.info("closing {} for cause {}", name, cause);
            try {
                close(name, index);
            } catch (final IOException e) {
                LOGGER.error("I/O exception when evicting " + name, e);
            }
        }
    }

    private class IndexWeigher implements Weigher<String, Index> {

        @Override
        public @NonNegative int weigh(String key, Index value) {
            // Pin active indexes
            return value.isActive() ? 0 : 1;
        }
    }

    private class AsyncIndexLoader implements AsyncCacheLoader<String, Index> {

        @Override
        public CompletableFuture<? extends Index> asyncLoad(String name, Executor executor) throws Exception {
            final CompletableFuture<Index> future = new CompletableFuture<Index>();

            executor.execute(() -> {
                LOGGER.info("opening {}", name);
                final Path path = indexPath(name);
                Index result;
                try {
                    final IndexDefinition indexDefinition = loadIndexDefinition(name);
                    final Analyzer analyzer = Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
                    final Directory dir = new DirectIODirectory(FSDirectory.open(path.resolve("9")));
                    final IndexWriterConfig config = new IndexWriterConfig(analyzer);
                    config.setUseCompoundFile(false);
                    final IndexWriter writer = new IndexWriter(dir, config);
                    final long updateSeq = getSeq(writer, "update_seq");
                    final long purgeSeq = getSeq(writer, "purge_seq");
                    final SearcherManager searcherManager = new SearcherManager(writer, searcherFactory);
                    result = new Lucene9Index(analyzer, writer, updateSeq, purgeSeq, searcherManager);
                    future.complete(result);
                } catch (IOException e) {
                    future.completeExceptionally(e);
                }
            });

            return future;
        }

        @Override
        public CompletableFuture<? extends Index> asyncReload(String name, Index index, Executor executor)
                throws Exception {
            executor.execute(() -> {
                if (index.tryAcquire()) {
                    try {
                        if (index.commit()) {
                            LOGGER.info("committed {}", name);
                        }
                    } catch (final IOException e) {
                        LOGGER.warn("I/O exception while committing " + name, e);
                    } finally {
                        index.release();
                    }
                }
            });
            return CompletableFuture.completedFuture(index);
        }

        private long getSeq(final IndexWriter writer, final String key) throws IOException {
            final Iterable<Map.Entry<String, String>> commitData = writer.getLiveCommitData();
            if (commitData == null) {
                return 0L;
            }
            for (Map.Entry<String, String> entry : commitData) {
                if (entry.getKey().equals(key)) {
                    return Long.parseLong(entry.getValue());
                }
            }
            return 0L;
        }
    }

    private void close(final String name, final CompletableFuture<Index> future) throws IOException {
        final Index index = future.getNow(null);
        if (index != null) {
            close(name, index);
        }
    }

    private void close(final String name, final Index index) throws IOException {
        IOUtils.runAll(
                () -> {
                    if (index.tryAcquire()) {
                        try {
                            if (index.commit()) {
                                LOGGER.debug("committed {} before close", name);
                            }
                        } finally {
                            index.release();
                        }
                    }
                },
                () -> {
                    index.close();
                },
                () -> {
                    if (index.isDeleteOnClose()) {
                        IOUtils.rm(indexRootPath(name));
                    }
                });
    }

    private void assertSame(final IndexDefinition a, final IndexDefinition b) {
        if (!a.equals(b)) {
            throw new WebApplicationException("Index already exists", Status.EXPECTATION_FAILED);
        }
    }
}
