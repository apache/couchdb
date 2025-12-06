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

package org.apache.couchdb.nouveau.health;

import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.file.Path;
import java.util.concurrent.Executors;
import org.apache.couchdb.nouveau.core.IndexManager;
import org.apache.couchdb.nouveau.resources.IndexResource;
import org.apache.lucene.search.SearcherFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class IndexHealthCheckTest {

    @Test
    public void testIndexHealthCheck(@TempDir final Path tempDir) throws Exception {
        var manager = new IndexManager();
        manager.setCommitIntervalSeconds(30);
        manager.setIdleSeconds(60);
        manager.setMaxIndexesOpen(1);
        manager.setObjectMapper(new ObjectMapper());
        manager.setRootDir(tempDir);
        manager.setScheduledExecutorService(Executors.newScheduledThreadPool(2));
        manager.setSearcherFactory(new SearcherFactory());
        manager.start();

        try {
            var resource = new IndexResource(manager);
            var check = new IndexHealthCheck(resource);
            var result = check.check();
            assertTrue(result.isHealthy(), result.toString());
        } finally {
            manager.stop();
        }
    }
}
