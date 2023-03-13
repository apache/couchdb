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

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map.Entry;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.core.Cache.CacheFunction;
import org.apache.couchdb.nouveau.core.Cache.CacheLoader;
import org.apache.couchdb.nouveau.core.Cache.CacheUnloader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.codahale.metrics.Gauge;
import com.codahale.metrics.MetricRegistry;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.dropwizard.lifecycle.Managed;

/**
 * The central class of Nouveau, responsible for loading and unloading Lucene
 * indexes and making them available for query.
 */

public final class IndexManager implements Managed {

    private static final Logger LOGGER = LoggerFactory.getLogger(IndexManager.class);

    private int maxIndexesOpen;

    private int commitIntervalSeconds;

    private int idleSeconds;

    private int lockCount;

    private Path rootDir;

    private ObjectMapper objectMapper;

    private MetricRegistry metricRegistry;

    private ScheduledExecutorService scheduler;

    @SuppressWarnings("rawtypes")
    private Cache<String, Index> cache;

    @SuppressWarnings("rawtypes")
    public <R> R with(final String name, final IndexLoader loader, final CacheFunction<Index, R> userFun)
            throws IOException {
        if (!exists(name)) {
            throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
        }

        final CacheLoader<String, Index> cacheLoader = (n) -> {
            LOGGER.info("opening {}", n);
            final Path path = indexPath(n);
            final IndexDefinition indexDefinition = loadIndexDefinition(n);
            return loader.apply(path, indexDefinition);
        };

        final CacheFunction<Index, R> fun = (index) -> {
            if (index.needsCommit(commitIntervalSeconds, TimeUnit.SECONDS)) {
                scheduleCommit(name, loader);
            }
            return userFun.apply(index);
        };

        return cache.with(name, cacheLoader, cacheUnloader(), fun);
    }

    public void create(final String name, IndexDefinition indexDefinition) throws IOException {
        if (exists(name)) {
            throw new WebApplicationException("Index already exists", Status.EXPECTATION_FAILED);
        }
        // Validate index definiton
        // TODO luceneFor(indexDefinition).validate(indexDefinition);

        // Persist definition
        final Path path = indexDefinitionPath(name);
        if (Files.exists(path)) {
            throw new FileAlreadyExistsException(name + " already exists");
        }
        Files.createDirectories(path.getParent());
        objectMapper.writeValue(path.toFile(), indexDefinition);
    }

    public boolean exists(final String name) {
        return Files.exists(indexDefinitionPath(name));
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
        cache.remove(name, cacheUnloader());
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

    public int getLockCount() {
        return lockCount;
    }

    public void setLockCount(int lockCount) {
        this.lockCount = lockCount;
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
    @SuppressWarnings("rawtypes")
    public void start() throws IOException {
        cache = new Cache.Builder<String, Index>()
                .setMaxItems(maxIndexesOpen)
                .setLockCount(lockCount)
                .build();
        metricRegistry.register(name(IndexManager.class, "cache"), new Gauge<Integer>() {
            @Override
            public Integer getValue() {
                return cache.size();
            }
        });

        Runnable idler = () -> {
            for (final Entry<String, Index> entry : cache.entrySet()) {
                if (entry.getValue().isIdle(idleSeconds, TimeUnit.SECONDS)) {
                    try {
                        cache.remove(entry.getKey(), cacheUnloader());
                    } catch (final IOException e) {
                        LOGGER.warn("I/O exception while closing " + entry.getKey(), e);
                    }
                }
            }
        };
        scheduler.scheduleWithFixedDelay(idler, idleSeconds, idleSeconds, TimeUnit.SECONDS);
    }

    @Override
    public void stop() throws IOException {
        cache.close(cacheUnloader());
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
        throw new WebApplicationException(name + " attempts to escape from index root directory",
                Status.BAD_REQUEST);
    }

    @SuppressWarnings("rawtypes")
    private CacheUnloader<String, Index> cacheUnloader() {
        return (name, index) -> {
            if (!index.isDeleteOnClose()) {
                LOGGER.info("committing and closing {}", name);
                index.commit();
            } else {
                LOGGER.info("closing {}", name);
            }
            index.close();
            if (index.isDeleteOnClose()) {
                IOUtils.rm(indexRootPath(name));
                LOGGER.info("deleted on close {}", name);
            }
        };
    }

    private void scheduleCommit(final String name, final IndexLoader<?> loader) {
        scheduler.execute(() -> {
            try {
                with(name, loader, (index) -> {
                    LOGGER.info("committing {}", name);
                    index.commit();
                    return null;
                });
            } catch (final IOException e) {
                LOGGER.warn("I/O exception while committing " + name, e);
            }
        });
    }

}
