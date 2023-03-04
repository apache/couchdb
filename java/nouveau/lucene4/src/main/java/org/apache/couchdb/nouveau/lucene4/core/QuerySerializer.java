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

package org.apache.couchdb.nouveau.lucene4.core;

import java.io.IOException;

import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.MatchAllDocsQuery;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermQuery;

import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

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
        } else if (query instanceof MatchAllDocsQuery) {
            gen.writeStartObject();
            gen.writeStringField("@type", "match_all");
            gen.writeEndObject();
        } else if (query instanceof PhraseQuery) {
            final PhraseQuery phraseQuery = (PhraseQuery) query;
            gen.writeStartObject();
            gen.writeStringField("@type", "phrase");
            final Term[] terms = phraseQuery.getTerms();
            gen.writeStringField("field", terms[0].field());
            gen.writeFieldName("terms");
            gen.writeStartArray();
            for (final Term term : terms) {
                gen.writeString(term.text());
            }
            gen.writeEndArray();
            gen.writeEndObject();
        } else if (query instanceof BooleanQuery) {
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
        } else {
            throw new JsonGenerationException(query.getClass() + " not supported", gen);
        }
    }

}
