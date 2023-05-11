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
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.github.benmanes.caffeine.cache.Scheduler;
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
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.stream.Stream;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.eclipse.jetty.io.RuntimeIOException;
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

    private ScheduledExecutorService scheduler;

    private Cache<String, Index> cache;

    private StripedLock<String> lock;

    public <R> R with(final String name, final IndexLoader loader, final IndexFunction<Index, R> indexFun)
            throws IOException, InterruptedException {
        while (true) {
            if (!exists(name)) {
                throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
            }

            final Index index;
            try {
                index = cache.get(name, (n) -> {
                    LOGGER.info("opening {}", n);
                    final Path path = indexPath(n);
                    try {
                        final IndexDefinition indexDefinition = loadIndexDefinition(n);
                        return loader.apply(path, indexDefinition);
                    } catch (final IOException e) {
                        throw new RuntimeIOException(e);
                    }
                });
            } catch (final RuntimeIOException e) {
                throw (IOException) e.getCause();
            }

            if (index.tryAcquire(1, TimeUnit.SECONDS)) {
                try {
                    final R result = indexFun.apply(index);
                    if (index.needsCommit(commitIntervalSeconds, TimeUnit.SECONDS)) {
                        scheduler.execute(() -> {
                            if (index.tryAcquire()) {
                                try {
                                    LOGGER.debug("committing {}", name);
                                    try {
                                        index.commit();
                                    } catch (final IOException e) {
                                        LOGGER.warn("I/O exception while committing " + name, e);
                                    }
                                } finally {
                                    index.release();
                                }
                            }
                        });
                    }
                    return result;
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
        final Index index = cache.asMap().remove(name);
        if (index != null) {
            index.setDeleteOnClose(true);
            close(name, index);
        } else {
            IOUtils.rm(indexRootPath(name));
        }
    }

    @JsonProperty
    public int getMaxIndexesOpen() {
        return maxIndexesOpen;
    }

    public void setMaxIndexesOpen(int maxIndexesOpen) {
        this.maxIndexesOpen = maxIndexesOpen;
    }

    public int getCommitIntervalSeconds() {
        return commitIntervalSeconds;
    }

    public void setCommitIntervalSeconds(int commitIntervalSeconds) {
        this.commitIntervalSeconds = commitIntervalSeconds;
    }

    public int getIdleSeconds() {
        return idleSeconds;
    }

    public void setIdleSeconds(int idleSeconds) {
        this.idleSeconds = idleSeconds;
    }

    public void setScheduler(ScheduledExecutorService scheduler) {
        this.scheduler = scheduler;
    }

    public Path getRootDir() {
        return rootDir;
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

    @Override
    public void start() throws IOException {
        cache = Caffeine.newBuilder()
                .recordStats(() -> new MetricsStatsCounter(metricRegistry, name(IndexManager.class, "cache")))
                .initialCapacity(maxIndexesOpen)
                .maximumSize(maxIndexesOpen)
                .expireAfterAccess(Duration.ofSeconds(idleSeconds))
                .scheduler(Scheduler.systemScheduler())
                .evictionListener(new IndexEvictionListener())
                .build();
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
        final Path result = rootDir.resolve(name).normalize();
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

    private void close(final String name, final Index index) throws IOException {
        IOUtils.runAll(
                () -> {
                    if (index.tryAcquire()) {
                        try {
                            if (!index.isDeleteOnClose() && index.commit()) {
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
