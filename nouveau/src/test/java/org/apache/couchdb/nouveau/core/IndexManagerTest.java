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

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.file.Path;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.lucene9.ParallelSearcherFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class IndexManagerTest {

    private static IndexManager manager;
    private static ScheduledExecutorService executorService;

    @BeforeAll
    public static void setupManager(@TempDir Path path) throws Exception {
        executorService = Executors.newScheduledThreadPool(2);

        manager = new IndexManager();
        manager.setRootDir(path);
        manager.setObjectMapper(new ObjectMapper());
        manager.setCommitIntervalSeconds(10);
        manager.setScheduledExecutorService(executorService);
        manager.setSearcherFactory(new ParallelSearcherFactory(ForkJoinPool.commonPool()));
        manager.start();
    }

    @AfterAll
    public static void cleanup() throws Exception {
        executorService.shutdownNow();
        executorService.awaitTermination(5, TimeUnit.SECONDS);
        manager.stop();
    }

    @Test
    public void managerReturnsUsableIndex() throws Exception {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");
        manager.create("foo", indexDefinition);
        var searchRequest = new SearchRequest();
        searchRequest.setQuery("*:*");
        var searchResults = manager.with("foo", (index) -> index.search(searchRequest));
        assertThat(searchResults.getTotalHits()).isEqualTo(0);
    }

    @Test
    public void managerReopensAClosedIndex() throws Exception {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");

        manager.create("bar", indexDefinition);

        manager.with("bar", (index) -> {
            index.close();
            return null;
        });

        final boolean isOpen = manager.with("bar", (index) -> {
            return index.isOpen();
        });
        assertThat(isOpen);
    }
}
