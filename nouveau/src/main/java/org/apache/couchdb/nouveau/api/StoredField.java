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

package org.apache.couchdb.nouveau.api;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public final class StoredField extends Field {

    @NotNull
    @Schema(oneOf = {String.class, Double.class, byte[].class})
    private final Object value;

    public StoredField(@JsonProperty("name") final String name, @JsonProperty("value") final Object value) {
        super(name);
        if (!(value instanceof String || value instanceof Number || value instanceof byte[])) {
            throw new IllegalArgumentException(value + " must be a string, number or byte array");
        }
        this.value = value;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "StoredField [name=" + name + ", value=" + value + "]";
    }
}
