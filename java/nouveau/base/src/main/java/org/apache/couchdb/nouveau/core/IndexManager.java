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

import static java.util.concurrent.TimeUnit.SECONDS;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Stream;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
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

    private int lockCount;

    private Path rootDir;

    private ObjectMapper objectMapper;

    private ScheduledExecutorService scheduler;

    @SuppressWarnings("rawtypes")
    private Map<String, Index> cache;

    private Map<String, Collection<ScheduledFuture<?>>> scheduledFutures;

    // index open and closes occur under one of these object locks, determined
    // by the hashCode of the index's name.
    private ReadWriteLock[] locks;

    @SuppressWarnings("rawtypes")
    public Index acquire(final String name, final IndexLoader loader) throws IOException {
        if (!exists(name)) {
            throw new WebApplicationException("Index does not exist", Status.NOT_FOUND);
        }

        // Optimistic check.
        readLock(name).lock();
        try {
            final Index result;
            synchronized (cache) {
                result = cache.get(name);
            }
            if (result != null) {
                result.incRef();
                return result;
            }
        } finally {
            readLock(name).unlock();
        }

        writeLock(name).lock();
        try {
            final Index existingIndex;
            // non-first threads to get here need to check again.
            synchronized (cache) {
                existingIndex = cache.get(name);
            }
            if (existingIndex != null) {
                existingIndex.incRef();
                return existingIndex;
            }

            LOGGER.info("opening {}", name);
            final Path path = indexPath(name);
            final IndexDefinition indexDefinition = loadIndexDefinition(name);
            final Index newIndex = loader.apply(path, indexDefinition);

            final Runnable committer = () -> {
                try {
                    if (newIndex.commit()) {
                        LOGGER.info("committed {}", name);
                    }
                } catch (final IOException e) {
                    LOGGER.error("I/O exception when committing " + name, e);
                }
            };
            final ScheduledFuture<?> committerFuture = scheduler.scheduleWithFixedDelay(committer,
                    commitIntervalSeconds, commitIntervalSeconds, SECONDS);

            final Runnable idler = () -> {
                if (newIndex.secondsSinceLastUse() >= idleSeconds) {
                    try {
                        LOGGER.info("closing idle {}", name);
                        release(name, newIndex);
                    } catch (IOException e) {
                        LOGGER.error("I/O exception when closing idle " + name, e);
                    }
                }
            };
            final ScheduledFuture<?> idlerFuture = scheduler.scheduleWithFixedDelay(idler, idleSeconds, idleSeconds,
                    SECONDS);

            synchronized (cache) {
                cache.put(name, newIndex);
                scheduledFutures.put(name, List.of(committerFuture, idlerFuture));
            }
            newIndex.incRef();
            return newIndex;
        } finally {
            writeLock(name).unlock();
        }
    }

    @SuppressWarnings("rawtypes")
    public void release(final String name, final Index index) throws IOException {
        writeLock(name).lock();
        try {
            Collection<ScheduledFuture<?>> futures = null;
            synchronized (cache) {
                if (index.getRefCount() == 1) {
                    cache.remove(name);
                    futures = scheduledFutures.remove(name);
                }
            }
            if (futures != null) {
                for (ScheduledFuture<?> future : futures) {
                    future.cancel(false);
                }
                if (index.commit()) {
                    LOGGER.info("committed {}", name);
                }
            }
            doRelease(name, index);
        } finally {
            writeLock(name).unlock();
        }
    }

    @SuppressWarnings("rawtypes")
    private void doRelease(final String name, final Index index) throws IOException {
        index.decRef();
        if (!index.isOpen()) {
            LOGGER.info("closed {}", name);
            if (index.isDeleteOnClose()) {
                IOUtils.rm(indexRootPath(name));
                LOGGER.info("deleted {}", name);
            }
        }
    }

    private Lock writeLock(final String name) {
        return rwl(name).writeLock();
    }

    private Lock readLock(final String name) {
        return rwl(name).readLock();
    }

    private ReadWriteLock rwl(final String name) {
        return locks[Math.abs(name.hashCode()) % locks.length];
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

    @SuppressWarnings("rawtypes")
    private void deleteIndex(final String name) throws IOException {
        final Index index;
        readLock(name).lock();
        try {
            synchronized (cache) {
                index = cache.remove(name);
            }
            if (index == null) {
                IOUtils.rm(indexRootPath(name));
            } else {
                index.setDeleteOnClose(true);
            }
        } finally {
            readLock(name).unlock();
        }
        if (index != null) {
            release(name, index);
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

    public int getLockCount() {
        return lockCount;
    }

    public void setLockCount(int lockCount) {
        this.lockCount = lockCount;
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

    public void setScheduler(final ScheduledExecutorService scheduler) {
        this.scheduler = scheduler;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public void start() throws IOException {
        locks = new ReadWriteLock[lockCount];
        for (int i = 0; i < locks.length; i++) {
            locks[i] = new ReentrantReadWriteLock();
        }

        scheduledFutures = new HashMap<String, Collection<ScheduledFuture<?>>>(maxIndexesOpen);

        cache = new LinkedHashMap<String, Index>(maxIndexesOpen, 0.75f, true);

        final Runnable lruEnforcer = () -> {
            final List<Entry<String, Index>> evictees = new ArrayList<Entry<String, Index>>();
            synchronized (cache) {
                final int surplus = cache.size() - maxIndexesOpen;
                if (surplus > 0) {
                    final Iterator<Entry<String, Index>> it = cache.entrySet().iterator();
                    for (int i = 0; i < surplus; i++) {
                        evictees.add(it.next());
                    }
                }
            }
            for (Entry<String, Index> evictee : evictees) {
                LOGGER.info("evicting {}", evictee.getKey());
                try {
                    release(evictee.getKey(), evictee.getValue());
                } catch (final IOException e) {
                    LOGGER.error("error evicting " + evictee.getKey(), e);
                }
            }
        };
        scheduler.scheduleWithFixedDelay(lruEnforcer, 5, 5, SECONDS);
    }

    @Override
    @SuppressWarnings("rawtypes")
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

}
