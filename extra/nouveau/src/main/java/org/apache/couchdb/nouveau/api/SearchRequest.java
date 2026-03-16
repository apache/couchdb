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
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.couchdb.nouveau.core.ser.PrimitiveWrapper;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public record SearchRequest(
        @JsonProperty @NotNull String query,
        @JsonProperty @PositiveOrZero long minUpdateSeq,
        @JsonProperty @PositiveOrZero long minPurgeSeq,
        @JsonProperty Locale locale,
        @JsonProperty String partition,
        @JsonProperty @Positive Integer limit,
        @JsonProperty List<@NotEmpty String> sort,
        @JsonProperty List<@NotEmpty String> counts,
        @JsonProperty Map<@NotEmpty String, List<@NotNull DoubleRange>> ranges,
        @JsonProperty PrimitiveWrapper<?>[] after,
        @JsonProperty Integer topN) {

    public SearchRequest {
        if (topN == null) {
            topN = 10;
        }
        if (limit == null) {
            limit = 25;
        }
        if (locale == null) {
            locale = Locale.getDefault();
        }
    }

    public boolean hasPartition() {
        return partition != null;
    }

    public boolean hasSort() {
        return sort != null;
    }

    public boolean hasCounts() {
        return counts != null;
    }

    public boolean hasRanges() {
        return ranges != null;
    }
}
