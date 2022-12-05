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

import org.apache.lucene.util.BytesRef;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

public class BytesRefDeserializer extends StdDeserializer<BytesRef> {


    public BytesRefDeserializer() {
        this(null);
    }

    public BytesRefDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public BytesRef deserialize(final JsonParser parser, final DeserializationContext context)
    throws IOException, JsonProcessingException {
        JsonNode node = parser.getCodec().readTree(parser);
        return new BytesRef(node.binaryValue());
    }

}
