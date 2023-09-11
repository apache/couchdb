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
import jakarta.validation.constraints.Positive;
import java.util.OptionalLong;

public final class IndexInfoRequest {

    private final OptionalLong matchUpdateSeq;

    private final OptionalLong updateSeq;

    private final OptionalLong matchPurgeSeq;

    private final OptionalLong purgeSeq;

    public IndexInfoRequest(
            @JsonProperty("match_update_seq") @Positive final OptionalLong matchUpdateSeq,
            @JsonProperty("update_seq") @Positive final OptionalLong updateSeq,
            @JsonProperty("match_purge_seq") @Positive final OptionalLong matchPurgeSeq,
            @JsonProperty("purge_seq") @Positive final OptionalLong purgeSeq) {
        this.matchUpdateSeq = matchUpdateSeq;
        this.updateSeq = updateSeq;
        this.matchPurgeSeq = matchPurgeSeq;
        this.purgeSeq = purgeSeq;
    }

    public OptionalLong getMatchUpdateSeq() {
        return matchUpdateSeq;
    }

    public OptionalLong getUpdateSeq() {
        return updateSeq;
    }

    public OptionalLong getMatchPurgeSeq() {
        return matchPurgeSeq;
    }

    public OptionalLong getPurgeSeq() {
        return purgeSeq;
    }

    @Override
    public String toString() {
        return "IndexInfoRequest [matchUpdateSeq=" + matchUpdateSeq + ", updateSeq=" + updateSeq + ", matchPurgeSeq="
                + matchPurgeSeq + ", purgeSeq=" + purgeSeq + "]";
    }
}
