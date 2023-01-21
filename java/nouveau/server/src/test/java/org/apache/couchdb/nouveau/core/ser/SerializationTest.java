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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

import org.apache.couchdb.nouveau.api.After;
import org.apache.couchdb.nouveau.api.DoubleRange;
import org.apache.couchdb.nouveau.api.document.DoublePoint;
import org.apache.couchdb.nouveau.api.document.StoredDoubleField;
import org.apache.couchdb.nouveau.api.document.StoredStringField;
import org.apache.couchdb.nouveau.api.document.StringField;
import org.apache.couchdb.nouveau.api.document.TextField;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

public class SerializationTest {

    private static ObjectMapper mapper;

    @BeforeAll
    public static void setupMapper() {
        mapper = new ObjectMapper();
    }

    @Test
    public void testSerializeStringFieldStoreYES() throws Exception {
        final String expected = "{\"@type\":\"string\",\"name\":\"foo\",\"value\":\"bar\",\"stored\":true}";
        final String actual = mapper.writeValueAsString(new StringField("foo", "bar", true));
        assertEquals(expected, actual);
    }

    @Test
    public void testSerializeStringFieldStoreNO() throws Exception {
        final String expected = "{\"@type\":\"string\",\"name\":\"foo\",\"value\":\"bar\",\"stored\":false}";
        final String actual = mapper.writeValueAsString(new StringField("foo", "bar", false));
        assertEquals(expected, actual);
    }

    @Test
    public void testSerializeTextFieldStoreYES() throws Exception {
        final String expected = "{\"@type\":\"text\",\"name\":\"foo\",\"value\":\"bar\",\"stored\":true}";
        final String actual = mapper.writeValueAsString(new TextField("foo", "bar", true));
        assertEquals(expected, actual);
    }

    @Test
    public void testSerializeTextFieldStoreNO() throws Exception {
        final String expected = "{\"@type\":\"text\",\"name\":\"foo\",\"value\":\"bar\",\"stored\":false}";
        final String actual = mapper.writeValueAsString(new TextField("foo", "bar", false));
        assertEquals(expected, actual);
    }

    @Test
    public void testSerializeDoublePoint() throws Exception {
        final String expected = "{\"@type\":\"double_point\",\"name\":\"foo\",\"value\":[12.5]}";
        final String actual = mapper.writeValueAsString(new DoublePoint("foo", 12.5));
        assertEquals(expected, actual);
    }

    @Test
    public void testDeserializeDoublePoint1D() throws Exception {
        final String json = "{\"@type\":\"double_point\",\"name\":\"foo\",\"value\":[12.5]}";
        final DoublePoint point = mapper.readValue(json, DoublePoint.class);
        assertEquals("foo", point.getName());
        assertArrayEquals(new double[]{12.5}, point.getValue());
    }

    @Test
    public void testDeserializeDoublePoint2D() throws Exception {
        final String json = "{\"@type\":\"double_point\",\"name\":\"foo\",\"value\":[12.5,13.6]}";
        final DoublePoint point = mapper.readValue(json, DoublePoint.class);
        assertEquals("foo", point.getName());
        assertArrayEquals(new double[]{12.5, 13.6}, point.getValue());
    }

    @Test
    public void testSerializeStoredFieldString() throws Exception {
        final String expected = "{\"@type\":\"stored_string\",\"name\":\"foo\",\"value\":\"bar\"}";
        final String actual = mapper.writeValueAsString(new StoredStringField("foo", "bar"));
        assertEquals(expected, actual);
    }

    @Test
    public void testSerializeStoredFieldDouble() throws Exception {
        final String expected = "{\"@type\":\"stored_double\",\"name\":\"foo\",\"value\":12.5}";
        final String actual = mapper.writeValueAsString(new StoredDoubleField("foo", 12.5));
        assertEquals(expected, actual);
    }

    @Test
    public void testSerializeAfter() throws Exception {
        final After after = new After(
                Float.valueOf(1),
                Double.valueOf(2),
                Integer.valueOf(3),
                Long.valueOf(4),
                "foo",
                new byte[]{'b', 'a', 'r'});

        final String expected = "[{\"@type\":\"float\",\"value\":1.0},{\"@type\":\"double\",\"value\":2.0},{\"@type\":\"int\",\"value\":3},{\"@type\":\"long\",\"value\":4},{\"@type\":\"string\",\"value\":\"foo\"},{\"@type\":\"bytes\",\"value\":\"YmFy\"}]";
        final String actual = mapper.writeValueAsString(after);
        assertEquals(expected, actual);

        final After after2 = mapper.readValue(expected, After.class);

        for (int i = 0; i < after.getFields().length; i++) {
            assertThat(after.getFields()[i].getClass()).isEqualTo(after2.getFields()[i].getClass());
        }
    }

    @Test
    public void testSerializeDoubleRange() throws Exception {
        final String expected = "{\"label\":\"foo\",\"min\":12.5,\"max\":52.1,\"min_inclusive\":false,\"max_inclusive\":false}";
        final String actual = mapper.writeValueAsString(new DoubleRange("foo", 12.5, false, 52.1, false));
        assertEquals(expected, actual);
    }

    @Test
    public void testDeserializeDoubleRange() throws Exception {
        final String expected = "{\"label\":\"foo\",\"min\":12.5,\"max\":52.1,\"min_inclusive\":false,\"max_inclusive\":false}";
        final DoubleRange actual = mapper.readValue(expected, DoubleRange.class);
        assertEquals("foo", actual.getLabel());
        assertEquals(12.5, actual.getMin());
        assertEquals(false, actual.isMinInclusive());
        assertEquals(52.1, actual.getMax());
        assertEquals(false, actual.isMaxInclusive());
    }

    @Test
    public void testDeserializeDoubleRangeDefaults() throws Exception {
        final String expected = "{\"label\":\"foo\",\"min\":12.5,\"max\":52.1}";
        final DoubleRange actual = mapper.readValue(expected, DoubleRange.class);
        assertEquals("foo", actual.getLabel());
        assertEquals(12.5, actual.getMin());
        assertEquals(true, actual.isMinInclusive());
        assertEquals(52.1, actual.getMax());
        assertEquals(true, actual.isMaxInclusive());
    }

}
