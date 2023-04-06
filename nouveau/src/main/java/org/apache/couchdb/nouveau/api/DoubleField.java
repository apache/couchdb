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

import io.dropwizard.jackson.JsonSnakeCase;
import jakarta.validation.constraints.NotNull;

@JsonSnakeCase
public class DoubleField extends Field {

    @NotNull
    private final Double value;

    private final boolean stored;

    private final boolean facet;

    public DoubleField(@JsonProperty("name") final String name, @JsonProperty("value") final Double value,
            @JsonProperty("stored") final boolean stored, @JsonProperty("facet") final boolean facet) {
        super(name);
        this.value = value;
        this.stored = stored;
        this.facet = facet;
    }

    @JsonProperty
    public Double getValue() {
        return value;
    }

    @JsonProperty
    public boolean isStored() {
        return stored;
    }

    @JsonProperty
    public boolean isFacet() {
        return facet;
    }

    @Override
    public String toString() {
        return "DoubleField [name=" + name + ", value=" + value + ", stored=" + stored + ", facet=" + facet + "]";
    }

}