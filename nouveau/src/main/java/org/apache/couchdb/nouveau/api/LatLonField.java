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

public final class LatLonField extends Field {

    private final double lon;

    private final double lat;

    public LatLonField(@JsonProperty("name") final String name, @JsonProperty("lat") final double lat,
            @JsonProperty("lon") final double lon) {
        super(name);
        this.lat = lat;
        this.lon = lon;
    }

    public double getLat() {
        return lat;
    }

    public double getLon() {
        return lon;
    }

    @Override
    public String toString() {
        return "LatLonField [name=" + name + ", lon=" + lon + ", lat=" + lat + "]";
    }

}
