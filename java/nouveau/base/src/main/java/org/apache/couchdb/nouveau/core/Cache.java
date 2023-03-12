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

        private int maxItems = 10;
        private int lockCount = -1;

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
            return new Cache<K, V>(maxItems, lockCount == -1 ? maxItems * 10 : lockCount);
        }

    }

    private final int maxItems;
    private final Map<K, V> cache;
    private final ReadWriteLock[] locks;

    private Cache(
            final int maxItems, final int lockCount) {
        this.maxItems = maxItems;
        this.locks = new ReadWriteLock[lockCount];
        for (int i = 0; i < locks.length; i++) {
            this.locks[i] = new ReentrantReadWriteLock();
        }
        this.cache = new LinkedHashMap<K, V>(maxItems, 0.75f, true);
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
                    put(key, loader.load(key));
                }
                rwl.readLock().lock();
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

    public boolean remove(final K key, final CacheUnloader<K, V> unloader) throws IOException {
        Objects.requireNonNull(key);
        Objects.requireNonNull(unloader);

        final ReadWriteLock rwl = rwl(key);
        rwl.writeLock().lock();
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

    public Set<Entry<K, V>> entrySet() {
        synchronized (cache) {
            return Collections.unmodifiableSet(new HashSet<Entry<K,V>>(cache.entrySet()));
        }
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
