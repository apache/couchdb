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

import java.util.List;
import java.util.Map;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.dropwizard.jackson.JsonSnakeCase;

@JsonSnakeCase
public class SearchResults<T> {

    @Min(0)
    private long totalHits;

    @NotNull
    private String totalHitsRelation;

    @NotNull
    private List<@NotNull SearchHit<T>> hits;

    private Map<@NotNull String, Map<@NotNull String, Number>> counts;

    private Map<@NotNull String, Map<@NotNull String, Number>> ranges;

    public SearchResults() {
    }

    public void setTotalHits(final long totalHits) {
        this.totalHits = totalHits;
    }

    @JsonProperty
    public long getTotalHits() {
        return totalHits;
    }

    public String getTotalHitsRelation() {
        return totalHitsRelation;
    }

    public void setTotalHitsRelation(String totalHitsRelation) {
        this.totalHitsRelation = totalHitsRelation;
    }

    public void setHits(final List<SearchHit<T>> hits) {
        this.hits = hits;
    }

    @JsonProperty
    public List<SearchHit<T>> getHits() {
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
        return "SearchResults [hits=" + hits + ", totalHits=" + totalHits + ", counts=" + counts + ", ranges=" + ranges + "]";
    }

}
