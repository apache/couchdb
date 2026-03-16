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
import java.util.Locale;
import java.util.Map;
import org.apache.couchdb.nouveau.core.ser.PrimitiveWrapper;

public final class SearchRequestBuilder {

    private String query;

    private long minUpdateSeq;

    private long minPurgeSeq;

    private Locale locale;

    private String partition;

    private Integer limit;

    private List<String> sort;

    private List<String> counts;

    private Map<String, List<DoubleRange>> ranges;

    private PrimitiveWrapper<?>[] after;

    private Integer topN;

    public void setQuery(final String query) {
        this.query = query;
    }

    public void setMinUpdateSeq(final long minUpdateSeq) {
        this.minUpdateSeq = minUpdateSeq;
    }

    public void setMinPurgeSeq(final long minPurgeSeq) {
        this.minPurgeSeq = minPurgeSeq;
    }

    public void setPartition(final String partition) {
        this.partition = partition;
    }

    public void setLimit(final Integer limit) {
        this.limit = limit;
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

    public void setRanges(final Map<String, List<DoubleRange>> ranges) {
        this.ranges = ranges;
    }

    public void setTopN(final Integer topN) {
        this.topN = topN;
    }

    public void setAfter(final PrimitiveWrapper<?>[] after) {
        this.after = after;
    }

    public SearchRequest build() {
        return new SearchRequest(
                query, minUpdateSeq, minPurgeSeq, locale, partition, limit, sort, counts, ranges, after, topN);
    }
}
