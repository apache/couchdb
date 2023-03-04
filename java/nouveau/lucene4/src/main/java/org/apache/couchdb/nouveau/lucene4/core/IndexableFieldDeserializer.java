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

import org.apache.lucene.document.DoubleField;
import org.apache.lucene.document.SortedDocValuesField;
import org.apache.lucene.document.SortedSetDocValuesField;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.util.BytesRef;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;

class IndexableFieldDeserializer extends StdDeserializer<IndexableField> {

    IndexableFieldDeserializer() {
        this(null);
    }

    IndexableFieldDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public IndexableField deserialize(final JsonParser parser, final DeserializationContext context)
            throws IOException, JsonProcessingException {
        JsonNode node = parser.getCodec().readTree(parser);

        final String type = node.get("@type").asText();
        final String name = node.get("name").asText();

        switch (type) {
            case "double":
                return new DoubleField(name, node.get("value").doubleValue(), asStore(node));
            case "string":
                return new StringField(name, node.get("value").asText(), asStore(node));
            case "text":
                return new TextField(name, node.get("value").asText(), asStore(node));
            case "sorted_dv":
                return new SortedDocValuesField(name, bytesRef(node));
            case "sorted_set_dv":
                return new SortedSetDocValuesField(name, bytesRef(node));
            case "stored":
                if (node.get("value").isDouble()) {
                    return new StoredField(name, node.get("value").asDouble());
                }
                if (node.get("value").isTextual()) {
                    final JsonNode value = node.get("value");
                    if (node.has("encoded") && node.get("encoded").asBoolean()) {
                        return new StoredField(name, new BytesRef(value.binaryValue()));
                    } else {
                        return new StoredField(name, value.asText());
                    }
                }
                throw new JsonParseException(parser, node.get("value") + " not a valid value for a stored field");
        }
        throw new JsonParseException(parser, type + " not a valid type of field");
    }

    private Store asStore(JsonNode node) {
        if (node.has("stored")) {
            return node.get("stored").asBoolean() ? Store.YES : Store.NO;
        }
        return Store.NO;
    }

    private BytesRef bytesRef(final JsonNode node) throws IOException {
        final JsonNode value = node.get("value");
        if (node.has("encoded") && node.get("encoded").asBoolean()) {
            return new BytesRef(value.binaryValue());
        }
        return new BytesRef(value.asText());
    }

}
