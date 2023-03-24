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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import org.apache.couchdb.nouveau.api.After;

public class AfterSerializer extends StdSerializer<After> {

    public AfterSerializer() {
        this(null);
    }

    public AfterSerializer(Class<After> vc) {
        super(vc);
    }

    @Override
    public void serialize(final After after, final JsonGenerator gen, final SerializerProvider provider)
            throws IOException {
        // We ignore fieldDoc.score as it will be in the fields array if we're sorting for relevance.
        // We ignore fieldDoc.doc as _id is always the last field and is unique.
        gen.writeStartArray();
        // Preserve type information for correct deserialization of cursor.
        for (final Object o : after.getFields()) {
            gen.writeStartObject();
            if (o instanceof String) {
                gen.writeStringField("@type", "string");
                gen.writeStringField("value", (String) o);
            } else if (o instanceof byte[]) {
                final byte[] bytes = (byte[]) o;
                gen.writeStringField("@type", "bytes");
                gen.writeFieldName("value");
                gen.writeBinary(bytes);
            } else if (o instanceof Float) {
                gen.writeStringField("@type", "float");
                gen.writeNumberField("value", (Float) o);
            } else if (o instanceof Double) {
                gen.writeStringField("@type", "double");
                gen.writeNumberField("value", (Double) o);
            } else if (o instanceof Integer) {
                gen.writeStringField("@type", "int");
                gen.writeNumberField("value", (Integer) o);
            } else if (o instanceof Long) {
                gen.writeStringField("@type", "long");
                gen.writeNumberField("value", (Long) o);
            } else {
                throw new IOException(o.getClass() + " not supported");
            }
            gen.writeEndObject();
        }
        gen.writeEndArray();
    }

}
