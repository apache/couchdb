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

import com.fasterxml.jackson.annotation.JsonProperty;

import io.dropwizard.jackson.JsonSnakeCase;

@JsonSnakeCase
public class DoublePoint extends Field {

    private double[] value;

    public DoublePoint() {
    }

    public DoublePoint(final String name, final double... value) {
        super(name);
        this.value = value;
    }

    @JsonProperty
    public double[] getValue() {
        return value;
    }

    @JsonProperty
    public void setValue(double... value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "DoublePoint [name=" + name + ", value=" + Arrays.toString(value) + "]";
    }

}
