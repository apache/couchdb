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

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.As;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

@JsonTypeInfo(use = Id.NAME, include = As.PROPERTY, property = "@type")
@JsonSubTypes({
    @JsonSubTypes.Type(value = ByteArrayWrapper.class, name = "bytes"),
    @JsonSubTypes.Type(value = DoubleWrapper.class, name = "double"),
    @JsonSubTypes.Type(value = FloatWrapper.class, name = "float"),
    @JsonSubTypes.Type(value = IntWrapper.class, name = "int"),
    @JsonSubTypes.Type(value = LongWrapper.class, name = "long"),
    @JsonSubTypes.Type(value = StringWrapper.class, name = "string"),
})
public class PrimitiveWrapper<T> {

    private T value;

    public PrimitiveWrapper(T value) {
        this.value = value;
    }

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }
}
