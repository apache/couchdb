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
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class Cache<K, V> {

    private static final Logger LOGGER = LoggerFactory.getLogger(Cache.class);

    @FunctionalInterface
    public interface CacheLoader<K, V> {
        V load(final K key) throws IOException;
    }

    @FunctionalInterface
    public interface CacheUnloader<K, V> {
        void unload(final K key, final V value) throws IOException;
    }

    @FunctionalInterface
    public interface CacheFunction<V, R> {
        R apply(final V value) throws IOException;
    }

    public static class Builder<K, V> {

        private int idleSeconds = -1;
        private int maxItems = 10;
        private int lockCount = -1;

        public Builder<K, V> setIdleSeconds(final int idleSeconds) {
            if (idleSeconds < 1) {
                throw new IllegalArgumentException("idleSeconds must be at least 1");
            }
            this.idleSeconds = idleSeconds;
            return this;
        }

        public Builder<K, V> setMaxItems(final int maxItems) {
            if (maxItems < 1) {
                throw new IllegalArgumentException("maxItems must be at least 1");
            }
            this.maxItems = maxItems;
            return this;
        }

        public Builder<K, V> setLockCount(final int lockCount) {
            if (lockCount != -1 && lockCount < 1) {
                throw new IllegalArgumentException("lockCount must be at -1 for ergonomic default or greater than 1 for explicit setting");
            }
            this.lockCount = lockCount;
            return this;
        }

        public Cache<K, V> build() {
            return new Cache<K, V>(maxItems, idleSeconds, lockCount == -1 ? maxItems * 10 : lockCount);
        }

    }

    private static class CachedValue<V> {

        private final V value;
        private long lastUsed;

        private CachedValue(final V value) {
            this.value = value;
            this.lastUsed = now();
        }

        private void updateLastUsed() {
            this.lastUsed = now();
        }

        private boolean isIdle(final long idle, final TimeUnit unit) {
            Objects.requireNonNull(unit);
            return lastUsed < now() - unit.toNanos(idle);
        }

        private long now() {
            return System.nanoTime();
        }

    }

    private final int maxItems;
    private final int idleSeconds;
    private final Map<K, CachedValue<V>> cache;
    private final ReadWriteLock[] locks;

    private Cache(
            final int maxItems, final int idleSeconds, final int lockCount) {
        this.maxItems = maxItems;
        this.idleSeconds = idleSeconds;
        this.locks = new ReadWriteLock[lockCount];
        for (int i = 0; i < locks.length; i++) {
            this.locks[i] = new ReentrantReadWriteLock();
        }
        this.cache = new LinkedHashMap<K, CachedValue<V>>(maxItems, 0.75f, true);
    }

    public <R> R with(K key, final CacheLoader<K, V> loader, final CacheUnloader<K, V> unloader,
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
                remove(evictee, unloader);
            }
        }

        final ReadWriteLock rwl = rwl(key);
        rwl.readLock().lock();
        if (!containsKey(key)) {
            rwl.readLock().unlock();
            rwl.writeLock().lock();
            try {
                if (!containsKey(key)) {
                    final V value = loader.load(key);
                    put(key, new CachedValue<V>(value));
                }
                rwl.readLock().lock();
            } finally {
                rwl.writeLock().unlock();
            }
        }
        try {
            return function.apply(get(key).value);
        } finally {
            rwl.readLock().unlock();
        }
    }

    public boolean remove(final K key, final CacheUnloader<K, V> unloader) throws IOException {
        Objects.requireNonNull(key);
        Objects.requireNonNull(unloader);

        final ReadWriteLock rwl = rwl(key);
        rwl.writeLock().lock();
        try {
            final CachedValue<V> cachedValue = remove(key);
            if (cachedValue == null) {
                return false;
            }
            unloader.unload(key, cachedValue.value);
            return true;
        } finally {
            rwl.writeLock().unlock();
        }
    }

    public int size() {
        synchronized (cache) {
            return cache.size();
        }
    }

    public void close(final CacheUnloader<K, V> unloader) throws IOException {
        for (K key : cache.keySet()) {
            remove(key, unloader);
        }
        cache.clear();
    }

    private ReadWriteLock rwl(final K key) {
        return locks[Math.abs(key.hashCode()) % locks.length];
    }

    private boolean containsKey(final K key) {
        synchronized (cache) {
            return cache.containsKey(key);
        }
    }

    private CachedValue<V> get(final K key) {
        synchronized (cache) {
            return cache.get(key);
        }
    }

    private CachedValue<V> remove(final K key) {
        synchronized (cache) {
            return cache.remove(key);
        }
    }

    private CachedValue<V> put(final K key, final CachedValue<V> cachedValue) {
        synchronized (cache) {
            return cache.put(key, cachedValue);
        }
    }

}
