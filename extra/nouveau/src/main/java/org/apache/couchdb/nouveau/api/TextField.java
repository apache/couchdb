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
import jakarta.validation.constraints.NotNull;
import java.util.Objects;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public final class TextField extends Field {

    @NotNull
    private final String value;

    private final boolean store;

    public TextField(
            @JsonProperty("name") final String name,
            @JsonProperty("value") final String value,
            @JsonProperty("store") final boolean store) {
        super(name);
        this.value = Objects.requireNonNull(value);
        this.store = store;
    }

    @JsonProperty
    public String getValue() {
        return value;
    }

    @JsonProperty
    public boolean isStore() {
        return store;
    }

    @Override
    public String toString() {
        return "TextField [name=" + name + ", value=" + value + ", store=" + store + "]";
    }
}
