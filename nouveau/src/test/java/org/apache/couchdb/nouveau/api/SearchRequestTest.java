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
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class SearchRequestTest {

    private static ObjectMapper mapper;

    @BeforeAll
    public static void setupMapper() {
        mapper = new ObjectMapper();
    }

    @Test
    public void testSerialisation() throws Exception {
        SearchRequest request = asObject();
        final String expected = mapper.writeValueAsString(
                mapper.readValue(getClass().getResource("/fixtures/SearchRequest.json"), SearchRequest.class));
        assertThat(mapper.writeValueAsString(request)).isEqualTo(expected);
    }

    @Test
    public void testDeserialisation() throws Exception {
        SearchRequest request = asObject();
        assertThat(mapper.readValue(getClass().getResource("/fixtures/SearchRequest.json"), SearchRequest.class)
                        .toString())
                .isEqualTo(request.toString());
    }

    private SearchRequest asObject() {
        final SearchRequest result = new SearchRequest();
        result.setQuery("*:*");
        result.setLimit(10);
        result.setCounts(List.of("bar"));
        result.setRanges(Map.of("foo", List.of(new DoubleRange("0 to 100 inc", 0.0, true, 100.0, true))));
        return result;
    }
}
