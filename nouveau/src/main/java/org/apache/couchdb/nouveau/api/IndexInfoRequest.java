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

    private final OptionalLong updateSeq;

    private final OptionalLong purgeSeq;

    public IndexInfoRequest(
            @JsonProperty("update_seq") @Positive final OptionalLong updateSeq,
            @JsonProperty("purge_seq") @Positive final OptionalLong purgeSeq) {
        this.updateSeq = updateSeq;
        this.purgeSeq = purgeSeq;
    }

    public OptionalLong getUpdateSeq() {
        return updateSeq;
    }

    public OptionalLong getPurgeSeq() {
        return purgeSeq;
    }

    @Override
    public String toString() {
        return "IndexInfoRequest [updateSeq=" + updateSeq + ", purgeSeq=" + purgeSeq + "]";
    }
}
