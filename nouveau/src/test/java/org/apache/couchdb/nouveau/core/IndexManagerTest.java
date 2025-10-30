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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.lucene9.ParallelSearcherFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class IndexManagerTest {

    private Path rootDir;
    private IndexManager manager;
    private ScheduledExecutorService executorService;

    @BeforeEach
    public void setupManager(@TempDir Path path) throws Exception {
        executorService = Executors.newScheduledThreadPool(2);
        rootDir = path;

        manager = new IndexManager();
        manager.setRootDir(path);
        manager.setObjectMapper(new ObjectMapper());
        manager.setCommitIntervalSeconds(10);
        manager.setScheduledExecutorService(executorService);
        manager.setSearcherFactory(new ParallelSearcherFactory(ForkJoinPool.commonPool()));
        manager.start();
    }

    @AfterEach
    public void cleanup() throws Exception {
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

    @Test
    public void deleteAllRemovesIndexByName() throws Exception {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");

        assertThat(countIndexes()).isEqualTo(0);
        manager.create("bar", indexDefinition);
        assertThat(countIndexes()).isEqualTo(1);
        manager.deleteAll("bar", null);
        assertThat(countIndexes()).isEqualTo(0);
    }

    @Test
    public void deleteAllRemovesIndexByPath() throws Exception {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");

        assertThat(countIndexes()).isEqualTo(0);
        manager.create("foo/bar", indexDefinition);
        assertThat(countIndexes()).isEqualTo(1);
        manager.deleteAll("foo/bar", null);
        assertThat(countIndexes()).isEqualTo(0);
    }

    @Test
    public void deleteAllRemovesIndexByGlob() throws Exception {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");

        assertThat(countIndexes()).isEqualTo(0);
        manager.create("foo/bar", indexDefinition);
        assertThat(countIndexes()).isEqualTo(1);
        manager.deleteAll("foo/*", null);
        assertThat(countIndexes()).isEqualTo(0);
    }

    @Test
    public void deleteAllRemovesIndexByGlobExceptExclusions() throws Exception {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");

        assertThat(countIndexes()).isEqualTo(0);
        manager.create("foo/bar", indexDefinition);
        manager.create("foo/baz", indexDefinition);
        assertThat(countIndexes()).isEqualTo(2);
        manager.deleteAll("foo/*", List.of("bar"));
        assertThat(countIndexes()).isEqualTo(1);
    }

    private long countIndexes() throws IOException {
        try (var stream =
                Files.find(rootDir, 10, (p, attr) -> p.getFileName().toString().equals("index_definition.json"))) {
            return stream.count();
        }
    }
}
