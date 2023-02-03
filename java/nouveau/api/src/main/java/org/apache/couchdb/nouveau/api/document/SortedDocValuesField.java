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

import java.util.Arrays;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public final class SortedDocValuesField extends Field {

    private final byte[] value;

    @JsonCreator
    public SortedDocValuesField(@JsonProperty("name") final String name, @JsonProperty("value") final byte[] value) {
        super(name);
        this.value = value;
    }

    @JsonProperty
    public byte[] getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "SortedDocValuesField [name=" + name + ", value=" + Arrays.toString(value) + "]";
    }

}
