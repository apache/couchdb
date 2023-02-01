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
import java.util.stream.Stream;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.caffeine.MetricsStatsCounter;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.github.benmanes.caffeine.cache.Scheduler;

import io.dropwizard.lifecycle.Managed;

public final class IndexManager implements Managed {

    private static final Logger LOGGER = LoggerFactory.getLogger(IndexManager.class);

    private class IndexLoader implements CacheLoader<String, Index> {

        @Override
        public @Nullable Index load(@NonNull String name) throws Exception {
            final Path path = indexPath(name);
            final IndexDefinition indexDefinition = objectMapper.readValue(indexDefinitionPath(name).toFile(),
                    IndexDefinition.class);
            return lucene.open(path, indexDefinition);
        }

        @Override
        public @Nullable Index reload(@NonNull String name, @NonNull Index index) throws Exception {
            if (index.commit()) {
                LOGGER.info("{} committed.", name);
            }
            return index;
        }

    }

    private class IndexRemovalListener implements RemovalListener<String, Index> {

        public void onRemoval(String name, Index index, RemovalCause cause) {
            try {
                if (index.isOpen()) {
                    LOGGER.info("{} closing.", name);
                    index.close();
                    if (index.isDeleteOnClose()) {
                        IOUtils.rm(indexRootPath(name));
                    }
                }
            } catch (final IOException e) {
                LOGGER.error(index + " threw exception when closing", e);
            }
        }
    }

    @Min(1)
    private int maxIndexesOpen;

    @Min(1)
    private int commitIntervalSeconds;

    @Min(1)
    private int idleSeconds;

    @NotEmpty
    private Path rootDir;

    @NotNull
    private Lucene lucene;

    @NotNull
    private ObjectMapper objectMapper;

    private MetricRegistry metricRegistry;

    private LoadingCache<String, Index> cache;

    public Index acquire(final String name) throws IOException {
        if (!exists(name)) {
            throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
        }
        return cache.get(name);
    }

    public void invalidate(final String name) {
        cache.invalidate(name);
    }

    public void create(final String name, IndexDefinition indexDefinition) throws IOException {
        if (exists(name)) {
            throw new WebApplicationException("Index already exists", Status.EXPECTATION_FAILED);
        }
        // Validate index definiton
        lucene.validate(indexDefinition);

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
        final Index index = cache.getIfPresent(name);
        if (index == null) {
            IOUtils.rm(indexRootPath(name));
        } else {
            index.setDeleteOnClose(true);
            cache.invalidate(name);
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

    public void setLucene(final Lucene lucene) {
        this.lucene = lucene;
    }

    public void setObjectMapper(final ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public void setMetricRegistry(final MetricRegistry metricRegistry) {
        this.metricRegistry = metricRegistry;
    }

    @Override
    public void start() throws IOException {
        final IndexRemovalListener indexRemovalListener = new IndexRemovalListener();
        cache = Caffeine.newBuilder()
                .recordStats(() -> new MetricsStatsCounter(metricRegistry, "IndexManager"))
                .initialCapacity(maxIndexesOpen)
                .maximumSize(maxIndexesOpen)
                .expireAfterAccess(Duration.ofSeconds(idleSeconds))
                .expireAfterWrite(Duration.ofSeconds(idleSeconds))
                .refreshAfterWrite(Duration.ofSeconds(commitIntervalSeconds))
                .scheduler(Scheduler.systemScheduler())
                .removalListener(indexRemovalListener)
                .build(new IndexLoader());
    }

    @Override
    public void stop() {
        cache.invalidateAll();
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

}
