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
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.FuzzyQuery;
import org.apache.lucene.search.MatchAllDocsQuery;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.PrefixQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.RegexpQuery;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.WildcardQuery;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class QuerySerializationTest {

    private static ObjectMapper mapper;

    @BeforeAll
    public static void setup() {
        mapper = new ObjectMapper();
        mapper.registerModule(new Lucene9Module());
    }

    @Test
    public void basicTest() throws Exception {
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

    @Test
    public void deserializeJunk() throws Exception {
        assertThrows(JsonParseException.class, () -> {
            mapper.readValue("{}", Query.class);
        });
    }

    @Test
    public void deserializeTermQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"term\", \"field\":\"foo\", \"text\":\"bar\"}", Query.class))
                .isEqualTo(new TermQuery(new Term("foo", "bar")));
    }

    @Test
    public void deserializeWildcardQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"wildcard\", \"field\":\"foo\", \"text\":\"bar\"}", Query.class))
                .isEqualTo(new WildcardQuery(new Term("foo", "bar")));
    }

    @Test
    public void deserializePrefixQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"prefix\", \"field\":\"foo\", \"text\":\"bar\"}", Query.class))
                .isEqualTo(new PrefixQuery(new Term("foo", "bar")));
    }

    @Test
    public void deserializeRegexQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"regexp\", \"field\":\"foo\", \"text\":\"bar\"}", Query.class))
                .isEqualTo(new RegexpQuery(new Term("foo", "bar")));
    }

    @Test
    public void deserializeFuzzyQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"fuzzy\", \"field\":\"foo\", \"text\":\"bar\"}", Query.class))
                .isEqualTo(new FuzzyQuery(new Term("foo", "bar")));
    }

    @Test
    public void deserializePhraseQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"phrase\", \"field\":\"foo\", \"terms\": [\"bar\"]}", Query.class))
                .isEqualTo(new PhraseQuery.Builder().add(new Term("foo", "bar")).build());
    }

    @Test
    public void deserializeMatchAllQuery() throws Exception {
        assertThat(mapper.readValue("{\"@type\":\"match_all\"}", Query.class)).isEqualTo(new MatchAllDocsQuery());
    }
}
