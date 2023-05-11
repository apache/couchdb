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

import com.codahale.metrics.MetricRegistry;
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
        var scheduler = Executors.newSingleThreadScheduledExecutor();
        var manager = new IndexManager();
        manager.setObjectMapper(new ObjectMapper());
        manager.setMetricRegistry(new MetricRegistry());
        manager.setRootDir(tempDir);
        manager.setScheduler(scheduler);
        manager.start();
        try {
            var resource = new IndexResource(manager, new SearcherFactory());
            var check = new IndexHealthCheck(resource);
            assertTrue(check.check().isHealthy());
        } finally {
            scheduler.shutdown();
            manager.stop();
        }
    }
}
