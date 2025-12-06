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

package org.apache.couchdb.nouveau.lucene;

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response.Status;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;
import org.apache.couchdb.nouveau.api.DoubleField;
import org.apache.couchdb.nouveau.api.Field;
import org.apache.couchdb.nouveau.api.StoredField;
import org.apache.couchdb.nouveau.api.StringField;
import org.apache.couchdb.nouveau.api.TextField;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;

final class LuceneIndexSchema {

    public enum Type {
        STRING,
        TEXT,
        DOUBLE,
        STORED;

        private static Type fromField(final Field field) {
            if (field instanceof StringField) {
                return STRING;
            } else if (field instanceof TextField) {
                return TEXT;
            } else if (field instanceof DoubleField) {
                return DOUBLE;
            } else if (field instanceof StoredField) {
                return STORED;
            }
            throw new IllegalArgumentException(field + " not supported");
        }
    }

    private final ConcurrentMap<String, Type> map;

    private LuceneIndexSchema(Map<String, Type> map) {
        this.map = new ConcurrentHashMap<>(map);
        this.map.put("_id", Type.STRING);
    }

    public static LuceneIndexSchema emptySchema() {
        return new LuceneIndexSchema(new HashMap<String, Type>());
    }

    public static LuceneIndexSchema fromString(final String schemaStr) {
        Objects.requireNonNull(schemaStr);
        if (schemaStr.isEmpty()) {
            return emptySchema();
        }
        var map = Arrays.stream(schemaStr.split(","))
                .collect(Collectors.toMap(i -> i.split(":")[0], i -> Type.valueOf(i.split(":")[1])));
        return new LuceneIndexSchema(map);
    }

    public void update(final Collection<Field> fields) {
        Objects.requireNonNull(fields);
        for (var field : fields) {
            map.putIfAbsent(field.getName(), Type.fromField(field));
            assertType(field);
        }
    }

    public Type getType(final String fieldName) {
        return map.get(fieldName);
    }

    public void assertType(final Field field) {
        Objects.requireNonNull(field);
        var expectedType = Type.fromField(field);
        var actualType = map.get(field.getName());
        if (actualType == null) {
            throw new WebApplicationException("Unknown field " + field.getName(), Status.BAD_REQUEST);
        }
        if (expectedType != actualType) {
            throw new WebApplicationException(
                    String.format("field %s is of type %s not %s", field.getName(), expectedType, actualType),
                    Status.BAD_REQUEST);
        }
    }

    public Map<String, PointsConfig> toPointsConfigMap(final Locale locale) {
        Objects.requireNonNull(locale);
        var numberFormat = NumberFormat.getInstance(locale);
        var doublePointsConfig = new PointsConfig(numberFormat, Double.class);
        return map.entrySet().stream()
                .filter(e -> e.getValue() == Type.DOUBLE)
                .collect(Collectors.toMap(e -> e.getKey(), e -> doublePointsConfig));
    }

    @Override
    public String toString() {
        return map.entrySet().stream()
                .map(e -> String.format("%s:%s", e.getKey(), e.getValue()))
                .collect(Collectors.joining(","));
    }
}
