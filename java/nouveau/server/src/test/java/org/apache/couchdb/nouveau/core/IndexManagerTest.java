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

import java.nio.file.Path;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.core.lucene9.Lucene9;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

import com.codahale.metrics.MetricRegistry;

import io.dropwizard.testing.junit5.DropwizardExtensionsSupport;

@ExtendWith(DropwizardExtensionsSupport.class)
public class IndexManagerTest {

    @TempDir
    static Path tempDir;

    private IndexManager manager;

    @BeforeEach
    public void setup() throws Exception {
        manager = new IndexManager();
        manager.setMetricRegistry(new MetricRegistry());
        manager.setLucene(new Lucene9());
        manager.setCommitIntervalSeconds(5);
        manager.setObjectMapper(new ObjectMapper());
        manager.setRootDir(tempDir);
        manager.start();
    }

    @AfterEach
    public void cleanup() throws Exception {
        manager.stop();
    }

    @Test
    public void testCreate() throws Exception {
        final IndexDefinition def = new IndexDefinition("standard", null);
        manager.create("foo", def);
    }

}
