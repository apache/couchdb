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
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Map.Entry;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.Timer;

import static com.codahale.metrics.MetricRegistry.name;

/**
 * A generic cache with an enforced maximum entry system.
 *
 * A striped lock is used to ensure that no caller will observe
 * the entries while they are loading or unloading.
 */

public final class Cache<K, V> {

    // For opening an expensive resource
    @FunctionalInterface
    public interface CacheLoader<K, V> {
        V load(final K key) throws IOException;
    }

    // Called before unloading to give clients an opportunity to
    // do something expensive (as long as the cached item is still usable while
    // doing so).
    // Occurs under a non-exclusive read lock.
    @FunctionalInterface
    public interface CachePreunloader<K, V> {
        void preunload(final K key, final V value) throws IOException;
    }

    // For closing an open resource as cheaply as possible.
    // Occurs under an exclusive write lock.
    @FunctionalInterface
    public interface CacheUnloader<K, V> {
        void unload(final K key, final V value) throws IOException;
    }

    @FunctionalInterface
    public interface CacheFunction<V, R> {
        R apply(final V value) throws IOException;
    }

    public static class Builder<K, V> {

        private int maxItems = 10;
        private int lockCount = -1;
        private MetricRegistry metricRegistry;

        public Builder<K, V> setMaxItems(final int maxItems) {
            if (maxItems < 1) {
                throw new IllegalArgumentException("maxItems must be at least 1");
            }
            this.maxItems = maxItems;
            return this;
        }

        public Builder<K, V> setLockCount(final int lockCount) {
            if (lockCount != -1 && lockCount < 1) {
                throw new IllegalArgumentException(
                        "lockCount must be at -1 for ergonomic default or greater than 1 for explicit setting");
            }
            this.lockCount = lockCount;
            return this;
        }

        public Builder<K, V> setMetricRegistry(final MetricRegistry metricRegistry) {
            this.metricRegistry = Objects.requireNonNull(metricRegistry);
            return this;
        }

        public Cache<K, V> build() {
            return new Cache<K, V>(maxItems, lockCount == -1 ? maxItems * 10 : lockCount, metricRegistry);
        }

    }

    private final int maxItems;
    private final Map<K, V> cache;
    private final Timer readLockAcquisitionTimer;
    private final Timer writeLockAcquisitionTimer;
    private final ReadWriteLock[] locks;

    private Cache(
            final int maxItems, final int lockCount, final MetricRegistry metricRegistry) {
        this.maxItems = maxItems;

        readLockAcquisitionTimer = metricRegistry.timer(name(Cache.class, "readLockAcquire"));
        writeLockAcquisitionTimer = metricRegistry.timer(name(Cache.class, "writeLockAcquire"));

        this.locks = new ReadWriteLock[lockCount];
        for (int i = 0; i < locks.length; i++) {
            this.locks[i] = new ReentrantReadWriteLock();
        }
        this.cache = new LinkedHashMap<K, V>(maxItems, 0.75f, true);
    }

    public <R> R with(K key,
            final CacheLoader<K, V> loader,
            final CachePreunloader<K, V> preunloader,
            final CacheUnloader<K, V> unloader,
            final CacheFunction<V, R> function) throws IOException {
        Objects.requireNonNull(key);
        Objects.requireNonNull(loader);
        Objects.requireNonNull(function);

        // Process evictions
        while (size() > maxItems) {
            K evictee = null;
            synchronized (cache) {
                var it = cache.keySet().iterator();
                if (it.hasNext()) {
                    evictee = it.next();
                }
            }
            if (evictee != null) {
                remove(evictee, preunloader, unloader);
            }
        }

        final ReadWriteLock rwl = rwl(key);
        acquireReadLock(rwl);
        if (!containsKey(key)) {
            rwl.readLock().unlock();
            acquireWriteLock(rwl);
            try {
                if (!containsKey(key)) {
                    put(key, loader.load(key));
                }
                acquireReadLock(rwl);
            } finally {
                rwl.writeLock().unlock();
            }
        }
        try {
            return function.apply(get(key));
        } finally {
            rwl.readLock().unlock();
        }
    }

    public boolean remove(final K key, final CachePreunloader<K, V> preunloader, final CacheUnloader<K, V> unloader)
            throws IOException {
        Objects.requireNonNull(key);
        Objects.requireNonNull(unloader);

        final ReadWriteLock rwl = rwl(key);
        acquireReadLock(rwl);
        if (containsKey(key)) {
            try {
                preunloader.preunload(key, get(key));
            } finally {
                rwl.readLock().unlock();
            }
            acquireWriteLock(rwl);
            try {
                final V value = remove(key);
                if (value == null) {
                    return false;
                }
                unloader.unload(key, value);
                return true;
            } finally {
                rwl.writeLock().unlock();
            }
        }
        rwl.readLock().unlock();
        return false;
    }

    public int size() {
        synchronized (cache) {
            return cache.size();
        }
    }

    public void close(final CachePreunloader<K, V> preunloader, final CacheUnloader<K, V> unloader) throws IOException {
        for (K key : cache.keySet()) {
            remove(key, preunloader, unloader);
        }
        cache.clear();
    }

    public Set<Entry<K, V>> entrySet() {
        synchronized (cache) {
            return Collections.unmodifiableSet(new HashSet<Entry<K, V>>(cache.entrySet()));
        }
    }

    private Lock acquireReadLock(final ReadWriteLock rwl) {
        final Lock result = rwl.readLock();
        try (final Timer.Context context = readLockAcquisitionTimer.time()) {
            result.lock();
        }
        return result;
    }

    private Lock acquireWriteLock(final ReadWriteLock rwl) {
        final Lock result = rwl.writeLock();
        try (final Timer.Context context = writeLockAcquisitionTimer.time()) {
            result.lock();
        }
        return result;
    }

    private ReadWriteLock rwl(final K key) {
        return locks[Math.abs(key.hashCode()) % locks.length];
    }

    private boolean containsKey(final K key) {
        synchronized (cache) {
            return cache.containsKey(key);
        }
    }

    private V get(final K key) {
        synchronized (cache) {
            return cache.get(key);
        }
    }

    private V remove(final K key) {
        synchronized (cache) {
            return cache.remove(key);
        }
    }

    private V put(final K key, final V value) {
        synchronized (cache) {
            return cache.put(key, value);
        }
    }

}
