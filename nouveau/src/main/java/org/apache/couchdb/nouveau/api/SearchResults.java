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
import jakarta.validation.constraints.PositiveOrZero;
import java.util.List;
import java.util.Map;
import org.apache.lucene.search.TotalHits.Relation;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class SearchResults {

    @PositiveOrZero
    private long totalHits;

    @NotNull
    private Relation totalHitsRelation;

    @NotNull
    private List<@NotNull SearchHit> hits;

    private Map<@NotNull String, Map<@NotNull String, Number>> counts;

    private Map<@NotNull String, Map<@NotNull String, Number>> ranges;

    public SearchResults() {}

    public void setTotalHits(final long totalHits) {
        this.totalHits = totalHits;
    }

    @JsonProperty
    public long getTotalHits() {
        return totalHits;
    }

    public Relation getTotalHitsRelation() {
        return totalHitsRelation;
    }

    public void setTotalHitsRelation(Relation relation) {
        this.totalHitsRelation = relation;
    }

    public void setHits(final List<SearchHit> hits) {
        this.hits = hits;
    }

    @JsonProperty
    public List<SearchHit> getHits() {
        return hits;
    }

    public void setCounts(final Map<String, Map<String, Number>> counts) {
        this.counts = counts;
    }

    @JsonProperty
    public Map<String, Map<String, Number>> getCounts() {
        return counts;
    }

    public void setRanges(final Map<String, Map<String, Number>> ranges) {
        this.ranges = ranges;
    }

    @JsonProperty
    public Map<String, Map<String, Number>> getRanges() {
        return ranges;
    }

    @Override
    public String toString() {
        return "SearchResults [hits=" + hits + ", totalHits=" + totalHits + ", counts=" + counts + ", ranges=" + ranges
                + "]";
    }
}
