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

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public record IndexDefinition(
        @Min(LEGACY_LUCENE_VERSION) @Max(LATEST_LUCENE_VERSION) OptionalInt luceneVersion,
        @NotEmpty String defaultAnalyzer,
        Optional<Map<@NotEmpty String, @NotEmpty String>> fieldAnalyzers,
        @NotNull OptionalLong initialPurgeSeq) {

    public static final int LEGACY_LUCENE_VERSION = 9;
    public static final int LATEST_LUCENE_VERSION = 10;

    public IndexDefinition() {
        this(OptionalInt.of(LATEST_LUCENE_VERSION), "standard", Optional.empty(), OptionalLong.empty());
    }

    @JsonIgnore
    public int luceneVersionAsInt() {
        return luceneVersion.orElse(LEGACY_LUCENE_VERSION);
    }

    @JsonIgnore
    public boolean isLatestVersion() {
        return luceneVersion.getAsInt() == LATEST_LUCENE_VERSION;
    }

    @JsonIgnore
    public long initialPurgeSeqAsLong() {
        return initialPurgeSeq.orElse(0L);
    }
}
