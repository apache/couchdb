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

import org.apache.lucene.document.BinaryDocValuesField;
import org.apache.lucene.document.DoubleDocValuesField;
import org.apache.lucene.document.DoublePoint;
import org.apache.lucene.document.FloatDocValuesField;
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

enum SupportedType {

    binary_dv(BinaryDocValuesField.class),
    double_dv(DoubleDocValuesField.class),
    double_point(DoublePoint.class),
    float_dv(FloatDocValuesField.class),
    float_point(FloatPoint.class),
    int_point(IntPoint.class),
    latlon_dv(LatLonDocValuesField.class),
    latlon_point(LatLonPoint.class),
    long_point(LongPoint.class),
    sorted_dv(SortedDocValuesField.class),
    sorted_numeric_dv(SortedNumericDocValuesField.class),
    sorted_set_dv(SortedSetDocValuesField.class),
    stored_binary(StoredField.class),
    stored_double(StoredField.class),
    stored_string(StoredField.class),
    string(StringField.class),
    text(TextField.class),
    xy_dv(XYDocValuesField.class),
    xy_point(XYPointField.class);

    private final Class<? extends IndexableField> clazz;

    private SupportedType(final Class<? extends IndexableField> clazz) {
        this.clazz = clazz;
    }

    public static SupportedType fromField(final IndexableField field) {
        if (field instanceof StoredField) {
            final StoredField storedField = (StoredField) field;
            if (storedField.numericValue() != null) {
                return stored_double;
            } else if (storedField.stringValue() != null) {
                return stored_string;
            } else if (storedField.binaryValue() != null) {
                return stored_binary;
            }
        }
        for (final SupportedType t : SupportedType.values()) {
            if (t.clazz.isAssignableFrom(field.getClass())) {
               return t;
           }
        }
        throw new IllegalArgumentException(field + " is not a supported type");
    }

}
