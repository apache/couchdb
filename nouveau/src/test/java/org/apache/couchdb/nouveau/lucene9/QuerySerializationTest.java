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

package org.apache.couchdb.nouveau.lucene9;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermQuery;
import org.junit.jupiter.api.Test;

public class QuerySerializationTest {

    @Test
    public void basicTest() throws Exception {
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new Lucene9Module());

        final BooleanQuery.Builder builder = new BooleanQuery.Builder();
        builder.add(new TermQuery(new Term("foo", "bar")), Occur.MUST);
        builder.add(new TermQuery(new Term("foo", "bar")), Occur.MUST_NOT);
        builder.add(new TermQuery(new Term("foo", "bar")), Occur.SHOULD);
        builder.add(new PhraseQuery("bar", "foo", "bar", "baz"), Occur.MUST);
        final Query query = builder.build();

        final String expected =
                "{\"@type\":\"boolean\",\"clauses\":[{\"query\":{\"@type\":\"term\",\"field\":\"foo\",\"term\":\"bar\"},\"occur\":\"must\"},{\"query\":{\"@type\":\"term\",\"field\":\"foo\",\"term\":\"bar\"},\"occur\":\"must_not\"},{\"query\":{\"@type\":\"term\",\"field\":\"foo\",\"term\":\"bar\"},\"occur\":\"should\"},{\"query\":{\"@type\":\"phrase\",\"field\":\"bar\",\"terms\":[\"foo\",\"bar\",\"baz\"],\"slop\":0},\"occur\":\"must\"}]}";
        assertThat(mapper.writeValueAsString(query)).isEqualTo(expected);
    }
}
