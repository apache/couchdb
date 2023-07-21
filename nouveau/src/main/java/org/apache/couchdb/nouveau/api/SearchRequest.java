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
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.couchdb.nouveau.core.ser.PrimitiveWrapper;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class SearchRequest {

    @NotNull
    private String query;

    private Locale locale;

    private String partition;

    @Positive
    private int limit = 25;

    private List<@NotEmpty String> sort;

    private List<@NotEmpty String> counts;

    private Map<@NotEmpty String, List<@NotNull DoubleRange>> ranges;

    private PrimitiveWrapper<?>[] after;

    @Min(1)
    @Max(100)
    private int topN = 10;

    public SearchRequest() {
        // Jackson deserialization
    }

    public void setQuery(final String query) {
        this.query = query;
    }

    @JsonProperty
    public String getQuery() {
        return query;
    }

    public void setLocale(final Locale locale) {
        this.locale = locale;
    }

    @JsonProperty
    public Locale getLocale() {
        return locale;
    }

    public void setPartition(final String partition) {
        this.partition = partition;
    }

    @JsonProperty
    public String getPartition() {
        return partition;
    }

    public boolean hasPartition() {
        return partition != null;
    }

    public void setLimit(final int limit) {
        this.limit = limit;
    }

    @JsonProperty
    public int getLimit() {
        return limit;
    }

    public boolean hasSort() {
        return sort != null;
    }

    @JsonProperty
    public List<String> getSort() {
        return sort;
    }

    public void setSort(List<String> sort) {
        this.sort = sort;
    }

    public boolean hasCounts() {
        return counts != null;
    }

    public void setCounts(final List<String> counts) {
        this.counts = counts;
    }

    @JsonProperty
    public List<String> getCounts() {
        return counts;
    }

    public boolean hasRanges() {
        return ranges != null;
    }

    public void setRanges(final Map<String, List<DoubleRange>> ranges) {
        this.ranges = ranges;
    }

    @JsonProperty
    public Map<String, List<DoubleRange>> getRanges() {
        return ranges;
    }

    public void setTopN(final int topN) {
        this.topN = topN;
    }

    @JsonProperty
    public int getTopN() {
        return topN;
    }

    public void setAfter(final PrimitiveWrapper<?>[] after) {
        this.after = after;
    }

    @JsonProperty
    public PrimitiveWrapper<?>[] getAfter() {
        return after;
    }

    @Override
    public String toString() {
        return "SearchRequest [query=" + query + ", locale=" + locale + ", sort=" + sort + ", limit=" + limit
                + ", after=" + after + ", counts=" + counts + ", ranges=" + ranges + "]";
    }
}
