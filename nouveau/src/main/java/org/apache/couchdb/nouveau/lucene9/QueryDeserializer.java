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

import java.io.IOException;
import java.util.Iterator;

import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.FuzzyQuery;
import org.apache.lucene.search.MatchAllDocsQuery;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.PrefixQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.RegexpQuery;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.WildcardQuery;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

public class QueryDeserializer extends StdDeserializer<Query> {

    public QueryDeserializer() {
        this(null);
    }

    public QueryDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public Query deserialize(final JsonParser parser, final DeserializationContext context)
            throws IOException, JsonProcessingException {
        return deserializeNode(parser, context, parser.getCodec().readTree(parser));
    }

    private Query deserializeNode(final JsonParser parser, final DeserializationContext context, final JsonNode node)
            throws IOException, JsonProcessingException {
        final String type = node.get("@type").asText();
        switch (type) {
            case "term": {
                final String field = node.get("field").asText();
                final String text = node.get("text").asText();
                return new TermQuery(new Term(field, text));
            }
            case "boolean": {
                if (!node.get("clauses").isArray()) {
                    throw new JsonParseException(parser, "boolean clauses must be an array");
                }
                final BooleanQuery.Builder builder = new BooleanQuery.Builder();
                final Iterator<JsonNode> it = node.get("clauses").elements();
                while (it.hasNext()) {
                    final Query q = deserializeNode(parser, context, it.next());
                    builder.add(q, null);
                }
                return builder.build();
            }
            case "wildcard": {
                final String field = node.get("field").asText();
                final String text = node.get("text").asText();
                return new WildcardQuery(new Term(field, text));
            }
            case "phrase": {
                final String field = node.get("field").asText();
                if (!node.get("terms").isArray()) {
                    throw new JsonParseException(parser, "phrase terms must be an array");
                }
                final PhraseQuery.Builder builder = new PhraseQuery.Builder();
                final Iterator<JsonNode> it = node.get("terms").elements();
                while (it.hasNext()) {
                    builder.add(new Term(field, it.next().asText()));
                }
                builder.setSlop(node.get("slop").asInt());
                return builder.build();
            }
            case "prefix": {
                final String field = node.get("field").asText();
                final String text = node.get("text").asText();
                return new PrefixQuery(new Term(field, text));
            }
            case "fuzzy": {
                final String field = node.get("field").asText();
                final String text = node.get("text").asText();
                final int maxEdits = node.get("max_edits").asInt();
                final int prefixLength = node.get("prefix_length").asInt();
                return new FuzzyQuery(new Term(field, text), maxEdits, prefixLength);
            }
            case "regexp": {
                final String field = node.get("field").asText();
                final String text = node.get("text").asText();
                return new RegexpQuery(new Term(field, text));
            }
            case "term_range": {

            }
            case "point_range": {

            }
            case "match_all":
            return new MatchAllDocsQuery();
        }
        throw new JsonParseException(parser, type + " not a supported query type");
    }

}
