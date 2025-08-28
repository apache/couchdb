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
import jakarta.validation.constraints.PositiveOrZero;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.couchdb.nouveau.core.ser.PrimitiveWrapper;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class SearchRequest {

    @NotNull
    private String query;

    @PositiveOrZero
    private long minUpdateSeq;

    @PositiveOrZero
    private long minPurgeSeq;

    private Locale locale;

    private String partition;

    @Positive
    private int limit = 25;

    private List<@NotEmpty String> sort;

    private List<@NotEmpty String> counts;

    private Map<@NotEmpty String, List<@NotNull DoubleRange>> ranges;

    private PrimitiveWrapper<?>[] after;

    @Min(1)
    @Max(1000)
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

    public void setMinUpdateSeq(final long minUpdateSeq) {
        this.minUpdateSeq = minUpdateSeq;
    }

    @JsonProperty
    public long getMinUpdateSeq() {
        return minUpdateSeq;
    }

    public void setMinPurgeSeq(final long minPurgeSeq) {
        this.minPurgeSeq = minPurgeSeq;
    }

    @JsonProperty
    public long getMinPurgeSeq() {
        return minPurgeSeq;
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
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((query == null) ? 0 : query.hashCode());
        result = prime * result + (int) (minUpdateSeq ^ (minUpdateSeq >>> 32));
        result = prime * result + (int) (minPurgeSeq ^ (minPurgeSeq >>> 32));
        result = prime * result + ((locale == null) ? 0 : locale.hashCode());
        result = prime * result + ((partition == null) ? 0 : partition.hashCode());
        result = prime * result + limit;
        result = prime * result + ((sort == null) ? 0 : sort.hashCode());
        result = prime * result + ((counts == null) ? 0 : counts.hashCode());
        result = prime * result + ((ranges == null) ? 0 : ranges.hashCode());
        result = prime * result + Arrays.hashCode(after);
        result = prime * result + topN;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        SearchRequest other = (SearchRequest) obj;
        if (query == null) {
            if (other.query != null) return false;
        } else if (!query.equals(other.query)) return false;
        if (minUpdateSeq != other.minUpdateSeq) return false;
        if (minPurgeSeq != other.minPurgeSeq) return false;
        if (locale == null) {
            if (other.locale != null) return false;
        } else if (!locale.equals(other.locale)) return false;
        if (partition == null) {
            if (other.partition != null) return false;
        } else if (!partition.equals(other.partition)) return false;
        if (limit != other.limit) return false;
        if (sort == null) {
            if (other.sort != null) return false;
        } else if (!sort.equals(other.sort)) return false;
        if (counts == null) {
            if (other.counts != null) return false;
        } else if (!counts.equals(other.counts)) return false;
        if (ranges == null) {
            if (other.ranges != null) return false;
        } else if (!ranges.equals(other.ranges)) return false;
        if (!Arrays.equals(after, other.after)) return false;
        if (topN != other.topN) return false;
        return true;
    }

    @Override
    public String toString() {
        return "SearchRequest [query=" + query + ", min_update_seq=" + minUpdateSeq + ", min_purge_seq=" + minPurgeSeq
                + ", locale=" + locale + ", sort=" + sort + ", limit=" + limit + ", after=" + after + ", counts="
                + counts + ", ranges=" + ranges + "]";
    }
}
