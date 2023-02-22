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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;

import static java.util.concurrent.TimeUnit.SECONDS;
import java.util.stream.Stream;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.eclipse.jetty.io.RuntimeIOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.dropwizard.lifecycle.Managed;

public final class IndexManager implements Managed {

    private static final Logger LOGGER = LoggerFactory.getLogger(IndexManager.class);

    private int maxIndexesOpen;

    private int commitIntervalSeconds;

    private int idleSeconds;

    private Path rootDir;

    private Map<Integer, Lucene> lucenes;

    private ObjectMapper objectMapper;

    private ScheduledExecutorService scheduler;

    private Map<String, Index> cache;

    private Map<String, ScheduledFuture<?>> commitFutures;

    private Object[] objectLocks;

    public Index acquire(final String name) throws IOException {
        if (!exists(name)) {
            throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
        }

        // Optimistic check.
        synchronized (cache) {
            final Index result = cache.get(name);
            if (result != null) {
                result.incRef();
                return result;
            }
        }

        synchronized (objectLock(name)) {
            // non-first threads to get here need to check again.
            synchronized (cache) {
                final Index result = cache.get(name);
                if (result != null) {
                    result.incRef();
                    return result;
                }
            }

            LOGGER.info("Opening {}", name);
            final Path path = indexPath(name);
            final IndexDefinition indexDefinition = objectMapper.readValue(indexDefinitionPath(name).toFile(),
                    IndexDefinition.class);
            final Index result = luceneFor(indexDefinition).open(path, indexDefinition);

            final ScheduledFuture<?> f = scheduler.scheduleWithFixedDelay(() -> {
                try {
                    if (result.commit()) {
                        LOGGER.info("Committed {}", name);
                    }
                } catch (final IOException e) {
                    LOGGER.error("I/O exception when committing " + name, e);
                }
            }, commitIntervalSeconds, commitIntervalSeconds, SECONDS);

            synchronized (cache) {
                cache.put(name, result);
                commitFutures.put(name, f);
                result.incRef();
                return result;
            }
        }
    }

    public void release(final String name, final Index index) throws IOException {
        synchronized (objectLock(name)) {
            ScheduledFuture<?> f = null;
            synchronized (cache) {
                if (index.getRefCount() == 1) {
                    cache.remove(name, index);
                    f = commitFutures.remove(name);
                }
            }
            if (f != null) {
                f.cancel(false);
                if (index.commit()) {
                    LOGGER.debug("Committed {}", name);
                }
            }
            doRelease(name, index);
        }
    }

    private void doRelease(final String name, final Index index) throws IOException {
        index.decRef();
        if (!index.isOpen()) {
            LOGGER.info("Closed {}", name);
            if (index.isDeleteOnClose()) {
                IOUtils.rm(indexRootPath(name));
                LOGGER.info("Deleted {}", name);
            }
        }
    }

    private Object objectLock(final String name) {
        return objectLocks[Math.abs(name.hashCode()) % objectLocks.length];
    }

    public void create(final String name, IndexDefinition indexDefinition) throws IOException {
        if (exists(name)) {
            throw new WebApplicationException("Index already exists", Status.EXPECTATION_FAILED);
        }
        // Validate index definiton
        luceneFor(indexDefinition).validate(indexDefinition);

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
        final Index index;
        synchronized (cache) {
            index = cache.get(name);
        }
        if (index == null) {
            IOUtils.rm(indexRootPath(name));
        } else {
            try (index) {
                index.setDeleteOnClose(true);
            }
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

    public Path getRootDir() {
        return rootDir;
    }

    public void setRootDir(Path rootDir) {
        this.rootDir = rootDir;
    }

    public void setLucenes(final Map<Integer, Lucene> lucenes) {
        this.lucenes = lucenes;
    }

    public void setObjectMapper(final ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public void setScheduler(final ScheduledExecutorService scheduler) {
        this.scheduler = scheduler;
    }

    @Override
    public void start() throws IOException {
        objectLocks = new Object[maxIndexesOpen / 10];
        for (int i = 0; i < objectLocks.length; i++) {
            objectLocks[i] = new Object();
        }

        commitFutures = new HashMap<String, ScheduledFuture<?>>(maxIndexesOpen);

        cache = new LinkedHashMap<String, Index>(maxIndexesOpen, 0.75f, true) {

            @Override
            protected boolean removeEldestEntry(java.util.Map.Entry<String, Index> eldest) {
                final boolean result = size() > maxIndexesOpen;
                if (result) {
                    LOGGER.info("Evicting {}", eldest.getKey());
                    try {
                        doRelease(eldest.getKey(), eldest.getValue());
                    } catch (final IOException e) {
                        throw new RuntimeIOException(e); // bleh.
                    }
                }
                return result;
            }

        };
    }

    @Override
    public void stop() throws IOException {
        synchronized (cache) {
            for (final Index index : cache.values()) {
                index.close();
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

    private Path indexRootPath(final String name) {
        final Path result = rootDir.resolve(name).normalize();
        if (result.startsWith(rootDir)) {
            return result;
        }
        throw new WebApplicationException(name + " attempts to escape from index root directory",
                Status.BAD_REQUEST);
    }

    private Lucene luceneFor(final IndexDefinition indexDefinition) {
        final int luceneMajor = indexDefinition.getLuceneMajor();
        final Lucene result = lucenes.get(luceneMajor);
        if (result == null) {
            throw new WebApplicationException("Lucene major version " + luceneMajor + " not valid", Status.BAD_REQUEST);
        }
        return result;
    }

}
