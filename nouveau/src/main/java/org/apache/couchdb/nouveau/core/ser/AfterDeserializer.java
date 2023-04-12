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

import org.apache.couchdb.nouveau.api.After;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.node.ArrayNode;

public class AfterDeserializer extends StdDeserializer<After> {

    public AfterDeserializer() {
        this(null);
    }

    public AfterDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public After deserialize(final JsonParser parser, final DeserializationContext context)
            throws IOException, JsonProcessingException {
        ArrayNode fieldNode = (ArrayNode) parser.getCodec().readTree(parser);
        final Object[] fields = new Object[fieldNode.size()];
        for (int i = 0; i < fields.length; i++) {
            final JsonNode field = fieldNode.get(i);
            switch (field.get("@type").asText()) {
                case "string":
                    fields[i] = field.get("value").asText();
                    break;
                case "bytes":
                    fields[i] = field.get("value").binaryValue();
                    break;
                case "float":
                    fields[i] = field.get("value").floatValue();
                    break;
                case "double":
                    fields[i] = field.get("value").doubleValue();
                    break;
                case "int":
                    fields[i] = field.get("value").intValue();
                    break;
                case "long":
                    fields[i] = field.get("value").longValue();
                    break;
                default:
                    throw new IOException("Unsupported field value: " + field);
            }
        }
        return new After(fields);
    }

}
