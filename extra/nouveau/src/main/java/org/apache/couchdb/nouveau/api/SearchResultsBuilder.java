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
import org.apache.lucene.search.TotalHits;

public final class SearchResultsBuilder {

    private TotalHits totalHits;

    private List<SearchHit> hits;

    private Map<String, Map<String, Number>> counts;

    private Map<String, Map<String, Number>> ranges;

    public void setTotalHits(final TotalHits totalHits) {
        this.totalHits = totalHits;
    }

    public void setHits(final List<SearchHit> hits) {
        this.hits = hits;
    }

    public void setCounts(final Map<String, Map<String, Number>> counts) {
        this.counts = counts;
    }

    public void setRanges(final Map<String, Map<String, Number>> ranges) {
        this.ranges = ranges;
    }

    public SearchResults build() {
        return new SearchResults(totalHits.value(), totalHits.relation(), hits, counts, ranges);
    }
}
