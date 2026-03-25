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

import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import org.apache.couchdb.nouveau.core.ser.PrimitiveWrapper;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public record SearchRequest(
        @NotNull String query,
        @PositiveOrZero long minUpdateSeq,
        @PositiveOrZero long minPurgeSeq,
        Optional<Locale> locale,
        Optional<String> partition,
        @Positive OptionalInt limit,
        @NotNull Optional<List<@NotEmpty String>> sort,
        @NotNull Optional<List<@NotEmpty String>> counts,
        @NotNull Optional<Map<@NotEmpty String, List<@NotNull DoubleRange>>> ranges,
        @NotNull Optional<PrimitiveWrapper<?>[]> after,
        @Min(1) @Max(1000) OptionalInt topN) {

    public int limitAsInt() {
        return limit.orElse(25);
    }

    public int topNAsInt() {
        return topN.orElse(10);
    }

    public boolean hasCounts() {
        return !counts.isEmpty();
    }

    public boolean hasRanges() {
        return !ranges.isEmpty();
    }

    public Locale getLocale() {
        return locale.orElse(Locale.getDefault());
    }

    public static class Builder {

        private String query;

        private long minUpdateSeq;

        private long minPurgeSeq;

        private Optional<Locale> locale = Optional.empty();

        private Optional<String> partition = Optional.empty();

        private OptionalInt limit = OptionalInt.empty();

        private Optional<List<String>> sort = Optional.empty();

        private Optional<List<String>> counts = Optional.empty();

        private Optional<Map<String, List<DoubleRange>>> ranges = Optional.empty();

        private Optional<PrimitiveWrapper<?>[]> after = Optional.empty();

        private OptionalInt topN = OptionalInt.empty();

        public Builder setQuery(final String query) {
            this.query = query;
            return this;
        }

        public Builder setMinUpdateSeq(final long minUpdateSeq) {
            this.minUpdateSeq = minUpdateSeq;
            return this;
        }

        public Builder setMinPurgeSeq(final long minPurgeSeq) {
            this.minPurgeSeq = minPurgeSeq;
            return this;
        }

        public Builder setLocale(final Locale locale) {
            this.locale = Optional.of(locale);
            return this;
        }

        public Builder setPartition(final String partition) {
            this.partition = Optional.of(partition);
            return this;
        }

        public Builder setLimit(final int limit) {
            this.limit = OptionalInt.of(limit);
            return this;
        }

        public Builder setSort(List<String> sort) {
            this.sort = Optional.of(sort);
            return this;
        }

        public Builder setCounts(final List<String> counts) {
            this.counts = Optional.of(counts);
            return this;
        }

        public Builder setRanges(final Map<String, List<DoubleRange>> ranges) {
            this.ranges = Optional.of(ranges);
            return this;
        }

        public Builder setTopN(final int topN) {
            this.topN = OptionalInt.of(topN);
            return this;
        }

        public Builder setAfter(final PrimitiveWrapper<?>[] after) {
            this.after = Optional.of(after);
            return this;
        }

        public SearchRequest build() {
            return new SearchRequest(
                    query, minUpdateSeq, minPurgeSeq, locale, partition, limit, sort, counts, ranges, after, topN);
        }
    }
}
