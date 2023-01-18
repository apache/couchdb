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

package org.apache.couchdb.nouveau.core.ser;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

import org.apache.lucene.facet.range.DoubleRange;

class DoubleRangeDeserializer extends StdDeserializer<DoubleRange> {

    public DoubleRangeDeserializer() {
        this(null);
    }

    public DoubleRangeDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public DoubleRange deserialize(final JsonParser parser, final DeserializationContext context)
    throws IOException, JsonProcessingException {
        JsonNode node = parser.getCodec().readTree(parser);
        final String label = node.get("label").asText();
        final double min = node.get("min").asDouble();
        final boolean minInc = node.has("inclusive_min") ? node.get("inclusive_min").asBoolean() : true;
        final double max = node.get("max").asDouble();
        final boolean maxInc = node.has("inclusive_max") ? node.get("inclusive_max").asBoolean() : true;
        return new DoubleRange(label, min, minInc, max, maxInc);
    }

}
