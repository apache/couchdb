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

package org.apache.couchdb.nouveau.lucene9.core;

import static org.assertj.core.api.Assertions.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.FSDirectory;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.github.benmanes.caffeine.cache.RemovalListener;
import com.github.benmanes.caffeine.cache.Scheduler;

public class CaffeineCacheTest {

    @Test
    public void testCaffeine(@TempDir Path tmpDir) throws Exception {
        final AtomicBoolean failed = new AtomicBoolean();

        final CacheLoader<Integer, IndexWriter> loader = (n) -> {
            var dir = FSDirectory.open(tmpDir.resolve("index-" + n));
            var conf = new IndexWriterConfig();
            var writer = new IndexWriter(dir, conf);
            writer.setLiveCommitData(Collections.singletonMap("foo", "bar").entrySet());
            writer.commit();
            return writer;
        };

        RemovalListener<Integer, IndexWriter> evictor = (n, writer, cause) -> {
            try {
                writer.close();
            } catch (IOException e) {
                failed.set(true);
                e.printStackTrace();
            }
        };

        final LoadingCache<Integer, IndexWriter> cache = Caffeine.newBuilder()
                .expireAfterAccess(1, TimeUnit.SECONDS)
                .maximumSize(10)
                .evictionListener(evictor)
                .scheduler(Scheduler.systemScheduler())
                .build(loader);

        final int nThreads = 20;
        final Thread[] threads = new Thread[nThreads];
        for (int i = 0; i < nThreads; i++) {
            threads[i] = new Thread(() -> {
                final Random testRandom = new Random();
                for (int j = 0; j < 10000; j++) {
                    final IndexWriter writer = cache.get(testRandom.nextInt(15));
                    try {
                        writer.addDocument(new Document());
                        writer.commit();
                    } catch (IOException e) {
                        failed.set(true);
                        return;
                    }
                }
            });
            threads[i].start();
        }

        for (int i = 0; i < nThreads; i++) {
            threads[i].join();
        }

        cache.invalidateAll();

        assertThat(failed).isFalse();
    }
}
