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

package org.apache.couchdb.nouveau.api.document;

import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public final class StringField extends Field {

    @NotNull
    private final String value;

    private final boolean stored;

    @JsonCreator
    public StringField(@JsonProperty("name") final String name, @JsonProperty("value") final String value,
            @JsonProperty("stored") final boolean stored) {
        super(name);
        this.value = value;
        this.stored = stored;
    }

    @JsonProperty
    public String getValue() {
        return value;
    }

    @JsonProperty
    public boolean isStored() {
        return stored;
    }

    @Override
    public String toString() {
        return "TextField [name=" + name + ", value=" + value + ", stored=" + stored + "]";
    }

}
