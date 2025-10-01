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

package org.apache.couchdb.nouveau.api;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class IndexDefinitionTest {

    private static ObjectMapper mapper;

    @BeforeAll
    public static void setup() {
        mapper = new ObjectMapper();
    }

    @Test
    public void legacyLuceneVersionIfMissing() throws Exception {
        var indexDefinition = mapper.readValue("{}", IndexDefinition.class);
        assertThat(indexDefinition.getLuceneVersion()).isEqualTo(IndexDefinition.LEGACY_LUCENE_VERSION);
    }

    @Test
    public void luceneVersionIsDeserializedIfPresent() throws Exception {
        var indexDefinition = mapper.readValue("{\"lucene_version\":10}", IndexDefinition.class);
        assertThat(indexDefinition.getLuceneVersion()).isEqualTo(10);
    }
}
