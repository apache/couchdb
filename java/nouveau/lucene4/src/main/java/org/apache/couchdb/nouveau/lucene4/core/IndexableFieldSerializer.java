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

import org.apache.lucene.document.StoredField;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.util.BytesRef;

import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

class IndexableFieldSerializer extends StdSerializer<IndexableField> {

    IndexableFieldSerializer() {
        this(null);
    }

    IndexableFieldSerializer(Class<IndexableField> vc) {
        super(vc);
    }

    @Override
    public void serialize(final IndexableField field, final JsonGenerator gen, final SerializerProvider provider)
            throws IOException {
        if (!(field instanceof StoredField)) {
            throw new JsonGenerationException(field.getClass() + " not supported", gen);
        }
        gen.writeStartObject();
        gen.writeStringField("@type", "stored");
        gen.writeStringField("name", field.name());
        if (field.numericValue() != null) {
            gen.writeNumberField("value", (double) field.numericValue());
        } else if (field.stringValue() != null) {
            gen.writeStringField("value", field.stringValue());
        } else if (field.binaryValue() != null) {
            final BytesRef bytesRef = field.binaryValue();
            gen.writeFieldName("value");
            gen.writeBinary(bytesRef.bytes, bytesRef.offset, bytesRef.length);
            gen.writeBooleanField("encoded", true);
        }
        gen.writeEndObject();
    }

}
