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
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.couchdb.nouveau.core.Cache.CacheLoader;
import org.apache.couchdb.nouveau.core.Cache.CacheUnloader;
import org.eclipse.jetty.io.RuntimeIOException;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IndexCacheTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(IndexCacheTest.class);

    @Test
    public void testCache() throws Exception {
        final Random cacheRandom = new Random();

        final CacheLoader<String, String> loader = (key) -> {
            try {
                Thread.sleep(cacheRandom.nextInt(20));
            } catch (InterruptedException e) {
                // ignored
            }
            return "loaded";
        };

        final CacheUnloader<String, String> unloader = (key, value) -> {
            try {
                Thread.sleep(cacheRandom.nextInt(20));
            } catch (InterruptedException e) {
                // ignored
            }
        };

        final Cache<String, String> cache = new Cache.Builder<String, String>()
                .setMaxItems(10)
                .build();

        final int nThreads = 20;
        final int keys = 3;
        final int loop = 1000;
        final AtomicInteger successes = new AtomicInteger(0);
        final AtomicInteger failures = new AtomicInteger(0);

        final Thread[] threads = new Thread[nThreads + 1];
        for (int i = 0; i < nThreads; i++) {
            threads[i] = new Thread(() -> {
                final Random testRandom = new Random();
                try {
                    for (int j = 0; j < loop; j++) {
                        if (testRandom.nextBoolean()) {
                            cache.with("foo-" + testRandom.nextInt(keys), loader, unloader, (v) -> {
                                if ("loaded".equals(v)) {
                                    successes.incrementAndGet();
                                } else {
                                    LOGGER.error("incorrect value: {}", v);
                                    failures.incrementAndGet();
                                }
                                return null;
                            });
                        } else {
                            cache.remove("foo-" + testRandom.nextInt(keys), unloader);
                            successes.incrementAndGet();
                        }
                    }
                } catch (IOException e) {
                    throw new RuntimeIOException(e);
                }
            });
            threads[i].start();
        }

        threads[nThreads] = new Thread(() -> {
            int done = 0;
            while (!Thread.currentThread().isInterrupted()) {
                final int s = successes.get();
                final int f = failures.get();
                done = s + f;
                LOGGER.info("{}% complete (successes: {}, failures: {})", (done * 100) / (loop * nThreads), s, f);
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        });
        threads[nThreads].start();

        for (int i = 0; i < nThreads; i++) {
            threads[i].join();
        }

        threads[nThreads].interrupt();

        LOGGER.info("successes: {}, failures: {}", successes.get(), failures.get());

        if (successes.get() < (loop * nThreads)) {
            throw new Exception("errors occurred");
        }
    }

}
