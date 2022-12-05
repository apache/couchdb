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

import org.apache.lucene.document.BinaryDocValuesField;
import org.apache.lucene.document.DoubleDocValuesField;
import org.apache.lucene.document.DoublePoint;
import org.apache.lucene.document.FloatDocValuesField;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.FloatPoint;
import org.apache.lucene.document.IntPoint;
import org.apache.lucene.document.LatLonDocValuesField;
import org.apache.lucene.document.LatLonPoint;
import org.apache.lucene.document.LongPoint;
import org.apache.lucene.document.SortedDocValuesField;
import org.apache.lucene.document.SortedNumericDocValuesField;
import org.apache.lucene.document.SortedSetDocValuesField;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.document.XYDocValuesField;
import org.apache.lucene.document.XYPointField;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.util.BytesRef;

class IndexableFieldDeserializer extends StdDeserializer<IndexableField> {

    public IndexableFieldDeserializer() {
        this(null);
    }

    public IndexableFieldDeserializer(Class<?> vc) {
        super(vc);
    }

    @Override
    public IndexableField deserialize(final JsonParser parser, final DeserializationContext context)
            throws IOException, JsonProcessingException {
        JsonNode node = parser.getCodec().readTree(parser);

        final SupportedType type = SupportedType.valueOf(node.get("@type").asText());
        final String name = node.get("name").asText();

        switch (type) {
            case binary_dv:
                return new BinaryDocValuesField(name, bytesRef(node));
            case double_point:
                return new DoublePoint(name, node.get("value").doubleValue());
            case float_dv:
                return new FloatDocValuesField(name, node.get("value").floatValue());
            case float_point:
                return new FloatPoint(name, node.get("value").floatValue());
            case latlon_dv:
                return new LatLonDocValuesField(name, node.get("lat").doubleValue(), node.get("lon").doubleValue());
            case latlon_point:
                return new LatLonPoint(name, node.get("lat").doubleValue(), node.get("lon").doubleValue());
            case int_point:
                return new IntPoint(name, node.get("value").intValue());
            case long_point:
                return new LongPoint(name, node.get("value").longValue());
            case xy_dv:
                return new XYDocValuesField(name, node.get("x").floatValue(), node.get("y").floatValue());
            case xy_point:
                return new XYPointField(name, node.get("x").floatValue(), node.get("y").floatValue());
            case string:
                return new StringField(name, node.get("value").asText(),
                        node.get("stored").asBoolean() ? Store.YES : Store.NO);
            case text:
                return new TextField(name, node.get("value").asText(),
                        node.get("stored").asBoolean() ? Store.YES : Store.NO);
            case stored_double:
                return new StoredField(name, node.get("value").asDouble());
            case stored_string:
                return new StoredField(name, node.get("value").asText());
            case stored_binary:
                return new StoredField(name, bytesRef(node));
            case sorted_set_dv:
                return new SortedSetDocValuesField(name, bytesRef(node));
            case sorted_dv:
                return new SortedDocValuesField(name, bytesRef(node));
            case sorted_numeric_dv:
                return new SortedNumericDocValuesField(name, node.get("value").longValue());
            case double_dv:
                return new DoubleDocValuesField(name, node.get("value").asDouble());
        }
        throw new IOException(type + " not a valid type of field");
    }

    private BytesRef bytesRef(final JsonNode node) throws IOException {
        final JsonNode value = node.get("value");
        if (node.has("encoded") && node.get("encoded").asBoolean()) {
            return new BytesRef(value.binaryValue());
        }
        return new BytesRef(value.asText());
    }

}
