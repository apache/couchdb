package org.apache.couchdb.nouveau.core.ser;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import org.apache.lucene.geo.GeoEncodingUtils;
import org.apache.lucene.geo.XYEncodingUtils;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.util.BytesRef;

class IndexableFieldSerializer extends StdSerializer<IndexableField> {

    public IndexableFieldSerializer() {
        this(null);
    }

    public IndexableFieldSerializer(Class<IndexableField> vc) {
        super(vc);
    }

    @Override
    public void serialize(final IndexableField field, final JsonGenerator gen, final SerializerProvider provider)
            throws IOException {
        final SupportedType type = SupportedType.fromField(field);
        gen.writeStartObject();
        gen.writeStringField("@type", type.toString());
        gen.writeStringField("name", field.name());
        switch (type) {
            case double_dv:
            case double_point:
            case stored_double:
                gen.writeNumberField("value", field.numericValue().doubleValue());
                break;
            case float_dv:
            case float_point:
                gen.writeNumberField("value", field.numericValue().floatValue());
                break;
            case int_point:
                gen.writeNumberField("value", field.numericValue().intValue());
                break;
            case latlon_dv:
            case latlon_point: {
                final long value = (Long) field.numericValue();
                gen.writeNumberField("lat", GeoEncodingUtils.decodeLatitude((int) (value >> 32)));
                gen.writeNumberField("lon", GeoEncodingUtils.decodeLongitude((int) (value & 0xFFFFFFFF)));
                break;
            }
            case long_point:
            case sorted_numeric_dv:
                gen.writeNumberField("value", field.numericValue().longValue());
                break;
            case binary_dv:
            case sorted_dv:
            case sorted_set_dv:
            case stored_binary: {
                final BytesRef bytesRef = field.binaryValue();
                gen.writeFieldName("value");
                gen.writeBinary(bytesRef.bytes, bytesRef.offset, bytesRef.length);
                gen.writeBooleanField("encoded", true);
                break;
            }
            case stored_string:
                gen.writeStringField("value", field.stringValue());
                break;
            case string:
            case text:
                gen.writeStringField("value", field.stringValue());
                gen.writeBooleanField("stored", field.fieldType().stored());
                break;
            case xy_dv:
            case xy_point: {
                final BytesRef bytesRef = field.binaryValue();
                gen.writeNumberField("x", XYEncodingUtils.decode(bytesRef.bytes, 0));
                gen.writeNumberField("y", XYEncodingUtils.decode(bytesRef.bytes, Integer.BYTES));
                break;
            }
        }
        gen.writeEndObject();
    }

}
