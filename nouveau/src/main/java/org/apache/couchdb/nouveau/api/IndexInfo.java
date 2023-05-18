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
import jakarta.validation.constraints.PositiveOrZero;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public final class IndexInfo {

    @PositiveOrZero
    private final long updateSeq;

    @PositiveOrZero
    private final long purgeSeq;

    @PositiveOrZero
    private final int numDocs;

    @PositiveOrZero
    private final long diskSize;

    public IndexInfo(
            @JsonProperty("update_seq") final long updateSeq,
            @JsonProperty("purge_seq") final long purgeSeq,
            @JsonProperty("num_docs") final int numDocs,
            @JsonProperty("disk_size") final long diskSize) {
        this.updateSeq = updateSeq;
        this.purgeSeq = purgeSeq;
        this.numDocs = numDocs;
        this.diskSize = diskSize;
    }

    public int getNumDocs() {
        return numDocs;
    }

    public long getDiskSize() {
        return diskSize;
    }

    public long getUpdateSeq() {
        return updateSeq;
    }

    public long getPurgeSeq() {
        return purgeSeq;
    }

    @Override
    public String toString() {
        return "IndexInfo [updateSeq=" + updateSeq + ", purgeSeq=" + purgeSeq + ", numDocs=" + numDocs + ", diskSize="
                + diskSize + "]";
    }
}
