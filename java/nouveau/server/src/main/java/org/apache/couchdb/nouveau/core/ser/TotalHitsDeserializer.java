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

package org.apache.couchdb.nouveau.core.ser;

import java.io.IOException;

import org.apache.lucene.search.TotalHits;
import org.apache.lucene.search.TotalHits.Relation;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

public class TotalHitsDeserializer extends StdDeserializer<TotalHits> {

    public TotalHitsDeserializer() {
        this(null);
    }

    public TotalHitsDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public TotalHits deserialize(final JsonParser parser, final DeserializationContext context)
            throws IOException, JsonProcessingException {
        JsonNode node = parser.getCodec().readTree(parser);
        final long value = node.get("value").asLong();
        final Relation relation = Relation.valueOf(node.get("relation").asText());
        return new TotalHits(value, relation);
    }

}
