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

import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import java.io.IOException;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.FuzzyQuery;
import org.apache.lucene.search.MatchAllDocsQuery;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.PointRangeQuery;
import org.apache.lucene.search.PrefixQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.RegexpQuery;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TermRangeQuery;
import org.apache.lucene.search.WildcardQuery;

class QuerySerializer extends StdSerializer<Query> {

    QuerySerializer() {
        this(null);
    }

    QuerySerializer(Class<Query> vc) {
        super(vc);
    }

    @Override
    public void serialize(final Query query, final JsonGenerator gen, final SerializerProvider provider)
            throws IOException {

        if (query instanceof TermQuery) {
            final TermQuery termQuery = (TermQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "term");
            gen.writeStringField("field", termQuery.getTerm().field());
            gen.writeStringField("term", termQuery.getTerm().text());
            gen.writeEndObject();
            return;
        }

        if (query instanceof BooleanQuery) {
            final BooleanQuery booleanQuery = (BooleanQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "boolean");
            gen.writeFieldName("clauses");
            gen.writeStartArray();
            for (final BooleanClause clause : booleanQuery.clauses()) {
                gen.writeStartObject();
                gen.writeFieldName("query");
                serialize(clause.getQuery(), gen, provider);
                gen.writeStringField("occur", clause.getOccur().name().toLowerCase());
                gen.writeEndObject();
            }
            gen.writeEndArray();
            gen.writeEndObject();
            return;
        }

        if (query instanceof WildcardQuery) {
            final WildcardQuery wildcardQuery = (WildcardQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "wildcard");
            gen.writeStringField("field", wildcardQuery.getField());
            gen.writeStringField("text", wildcardQuery.getTerm().text());
            gen.writeEndObject();
            return;
        }

        if (query instanceof PhraseQuery) {
            final PhraseQuery phraseQuery = (PhraseQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "phrase");
            gen.writeStringField("field", phraseQuery.getField());
            gen.writeFieldName("terms");
            gen.writeStartArray();
            for (final Term term : phraseQuery.getTerms()) {
                gen.writeString(term.text());
            }
            gen.writeEndArray();
            gen.writeNumberField("slop", phraseQuery.getSlop());
            gen.writeEndObject();
            return;
        }

        if (query instanceof PrefixQuery) {
            final PrefixQuery prefixQuery = (PrefixQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "prefix");
            gen.writeStringField("field", prefixQuery.getField());
            gen.writeStringField("text", prefixQuery.getPrefix().text());
            gen.writeEndObject();
            return;
        }

        if (query instanceof FuzzyQuery) {
            final FuzzyQuery fuzzyQuery = (FuzzyQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "fuzzy");
            gen.writeStringField("field", fuzzyQuery.getField());
            gen.writeStringField("text", fuzzyQuery.getTerm().text());
            gen.writeNumberField("max_edits", fuzzyQuery.getMaxEdits());
            gen.writeNumberField("prefix_length", fuzzyQuery.getPrefixLength());
            gen.writeEndObject();
            return;
        }

        if (query instanceof RegexpQuery) {
            final RegexpQuery regexpQuery = (RegexpQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "regexp");
            gen.writeStringField("field", regexpQuery.getField());
            gen.writeStringField("text", regexpQuery.getRegexp().text());
            gen.writeEndObject();
            return;
        }

        if (query instanceof TermRangeQuery) {
            final TermRangeQuery termRangeQuery = (TermRangeQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "term_range");
            gen.writeStringField("field", termRangeQuery.getField());
            gen.writeStringField("lower", termRangeQuery.getLowerTerm().utf8ToString());
            gen.writeBooleanField("includes_lower", termRangeQuery.includesLower());
            gen.writeStringField("upper", termRangeQuery.getUpperTerm().utf8ToString());
            gen.writeBooleanField("includes_upper", termRangeQuery.includesUpper());
            gen.writeEndObject();
            return;
        }

        if (query instanceof PointRangeQuery) {
            final PointRangeQuery pointRangeQuery = (PointRangeQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "point_range");
            gen.writeStringField("field", pointRangeQuery.getField());
            gen.writeBinaryField("lower", pointRangeQuery.getLowerPoint());
            gen.writeBinaryField("upper", pointRangeQuery.getUpperPoint());
            gen.writeNumberField("num_dims", pointRangeQuery.getNumDims());
            gen.writeEndObject();
        }

        if (query instanceof MatchAllDocsQuery) {
            gen.writeStartObject();
            gen.writeStringField("@type", "match_all");
            gen.writeEndObject();
            return;
        }

        throw new JsonGenerationException(query.getClass() + " not supported", gen);
    }
}
