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

import com.fasterxml.jackson.databind.ObjectMapper;
import io.dropwizard.lifecycle.Managed;
import jakarta.validation.constraints.PositiveOrZero;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response.Status;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The central class of Nouveau, responsible for loading and unloading Lucene
 * indexes and making them available for query.
 */
public final class IndexManager implements Managed {

    private class LRUMap extends LinkedHashMap<String, IndexHolder> {

        private LRUMap(final int capacity) {
            super(capacity, 0.75f, true);
        }
    }

    @FunctionalInterface
    public interface IndexFunction<V, R> {
        R apply(final V value) throws IOException;
    }

    private enum HolderState {
        NOT_LOADED,
        LOADED,
        UNLOADED
    }

    private static class IndexHolder {
        private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock(true);
        private ScheduledFuture<?> commitFuture;
        private HolderState state = HolderState.NOT_LOADED;
        private Index index;
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(IndexManager.class);

    @PositiveOrZero
    private int maxIndexesOpen;

    @PositiveOrZero
    private int commitIntervalSeconds;

    @PositiveOrZero
    private int idleSeconds;

    private Path rootDir;

    private ObjectMapper objectMapper;

    private ScheduledExecutorService schedulerExecutorService;

    private SearcherFactory searcherFactory;

    private LRUMap cache;

    private StripedLock<String> createLock;

    public <R> R with(final String name, final IndexFunction<Index, R> indexFun)
            throws IOException, InterruptedException {
        evictIfOverCapacity();

        retry:
        while (true) {
            if (!exists(name)) {
                throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
            }

            final IndexHolder holder;
            synchronized (cache) {
                holder = cache.computeIfAbsent(name, (k) -> new IndexHolder());
            }
            // CachedData pattern from ReentrantReadWriteLock javadoc
            holder.lock.readLock().lock();

            // Load if not already loaded or remove if Lucene closed the index elsewhere.
            if (holder.state == HolderState.NOT_LOADED
                    || (holder.state == HolderState.LOADED && !holder.index.isOpen())) {
                holder.lock.readLock().unlock();
                holder.lock.writeLock().lock();
                try {
                    if (holder.state == HolderState.LOADED && !holder.index.isOpen()) {
                        LOGGER.info("removing closed index {}", name);
                        holder.state = HolderState.UNLOADED;
                        holder.index = null;
                        synchronized (cache) {
                            cache.remove(name, holder);
                        }
                        continue retry;
                    }
                    if (holder.state == HolderState.NOT_LOADED) {
                        holder.index = load(name);
                        holder.commitFuture = this.schedulerExecutorService.scheduleWithFixedDelay(
                                commitFun(name, holder),
                                commitIntervalSeconds,
                                commitIntervalSeconds,
                                TimeUnit.SECONDS);
                        holder.state = HolderState.LOADED;
                    }
                    holder.lock.readLock().lock();
                } finally {
                    holder.lock.writeLock().unlock();
                }
            }

            try {
                switch (holder.state) {
                    case NOT_LOADED:
                        throw new IllegalStateException();
                    case UNLOADED:
                        Thread.sleep(1000);
                        continue retry;
                    case LOADED:
                        return indexFun.apply(holder.index);
                }
            } finally {
                holder.lock.readLock().unlock();
            }
        }
    }

    private void evictIfOverCapacity() throws IOException, InterruptedException {
        while (true) {
            final String candidate;
            synchronized (cache) {
                if (cache.size() <= maxIndexesOpen) {
                    return;
                }
                candidate = cache.entrySet().iterator().next().getKey();
            }
            unload(candidate, false);
        }
    }

    public void unload(final String name, final boolean forceDelete) throws IOException, InterruptedException {
        final IndexHolder holder;
        synchronized (cache) {
            holder = cache.computeIfAbsent(name, (k) -> new IndexHolder());
        }
        holder.lock.writeLock().lock();
        try {
            switch (holder.state) {
                case LOADED:
                    if (forceDelete) {
                        holder.index.setDeleteOnClose(true);
                    }
                    LOGGER.info("closing {}", name);
                    try {
                        close(name, holder);
                    } catch (final IOException e) {
                        LOGGER.error("I/O exception when evicting {}", name, e);
                    }
                    holder.state = HolderState.UNLOADED;
                    holder.index = null;
                    break;
                case NOT_LOADED:
                case UNLOADED:
                    if (forceDelete) {
                        IOUtils.rm(indexRootPath(name));
                    }
                    break;
            }
        } finally {
            synchronized (cache) {
                cache.remove(name, holder);
            }
            holder.lock.writeLock().unlock();
        }
    }

    public void create(final String name, IndexDefinition indexDefinition) throws IOException {
        if (exists(name)) {
            assertSame(indexDefinition, loadIndexDefinition(name));
            return;
        }

        final Lock lock = this.createLock.writeLock(name);
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
        LOGGER.info(
                "deleting indexes matching {} (excluding {})",
                path,
                exclusions == null || exclusions.isEmpty() ? "nothing" : exclusions);
        var parts = path.split("/");
        deleteAll(rootDir, parts, 0, exclusions);
    }

    private void deleteAll(final Path path, final String[] parts, final int index, final List<String> exclusions)
            throws IOException {
        // End of the path
        if (index == parts.length - 1) {
            try (var stream = Files.newDirectoryStream(path, parts[index])) {
                stream.forEach(p -> {
                    if (exclusions != null && exclusions.indexOf(p.getFileName().toString()) != -1) {
                        return;
                    }
                    final String relativeName = rootDir.relativize(p).toString();
                    try {
                        deleteIndex(relativeName);
                    } catch (final IOException | InterruptedException e) {
                        LOGGER.error("Exception deleting {}", p, e);
                    }
                    // Clean any newly empty directories.
                    do {
                        final File f = p.toFile();
                        if (f.isDirectory() && f.list().length == 0) {
                            f.delete();
                        }
                    } while ((p = p.getParent()) != null && !rootDir.equals(p));
                });
            }
            return;
        }
        // Recurse
        try (var stream = Files.newDirectoryStream(path, parts[index])) {
            stream.forEach(p -> {
                try {
                    deleteAll(p, parts, index + 1, exclusions);
                } catch (IOException e) {
                    LOGGER.warn("Exception during delete of " + rootDir.relativize(p), e);
                }
            });
        }
    }

    private void deleteIndex(final String name) throws IOException, InterruptedException {
        unload(name, true);
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

    public void setScheduledExecutorService(ScheduledExecutorService schedulerExecutorService) {
        this.schedulerExecutorService = schedulerExecutorService;
    }

    public void setSearcherFactory(final SearcherFactory searcherFactory) {
        this.searcherFactory = searcherFactory;
    }

    @Override
    public void start() throws IOException {
        Objects.requireNonNull(this.objectMapper, "objectMapper must not be null");
        Objects.requireNonNull(this.rootDir, "rootDir must not be null");
        Objects.requireNonNull(this.searcherFactory, "searcherFactory must not be null");
        Objects.requireNonNull(this.schedulerExecutorService, "schedulerExecutorService must not be null");

        this.cache = new LRUMap(this.maxIndexesOpen);
        this.createLock = new StripedLock<String>(100);
    }

    private Runnable commitFun(final String name, final IndexHolder holder) {
        return () -> {
            holder.lock.readLock().lock();
            try {
                if (holder.index.commit()) {
                    LOGGER.info("committed {}", name);
                }
            } catch (final IOException e) {
                LOGGER.warn("I/O exception while committing " + name, e);
            } finally {
                holder.lock.readLock().unlock();
            }
        };
    }

    @Override
    public void stop() throws IOException, InterruptedException {
        synchronized (cache) {
            var names = new HashSet<String>(cache.keySet());
            for (var name : names) {
                unload(name, false);
            }
            cache.clear();
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

    private Index load(final String name) throws IOException {
        LOGGER.info("opening {}", name);
        final Path path = indexPath(name);
        final IndexDefinition indexDefinition = loadIndexDefinition(name);
        final Analyzer analyzer = Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
        final Directory dir = new DirectIODirectory(FSDirectory.open(path.resolve("9")));
        final IndexWriterConfig config = new IndexWriterConfig(analyzer);
        config.setUseCompoundFile(false);
        final IndexWriter writer = new IndexWriter(dir, config);
        final long updateSeq = getSeq(writer, "update_seq");
        final long purgeSeq = getSeq(writer, "purge_seq");
        final SearcherManager searcherManager = new SearcherManager(writer, searcherFactory);
        return new Lucene9Index(analyzer, writer, updateSeq, purgeSeq, searcherManager);
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

    private void close(final String name, final IndexHolder holder) throws IOException {
        if (!holder.lock.isWriteLockedByCurrentThread()) {
            throw new IllegalStateException();
        }
        holder.commitFuture.cancel(true);
        IOUtils.runAll(
                () -> {
                    if (holder.index.commit()) {
                        LOGGER.debug("committed {} before close", name);
                    }
                },
                () -> {
                    holder.index.close();
                },
                () -> {
                    if (holder.index.isDeleteOnClose()) {
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
