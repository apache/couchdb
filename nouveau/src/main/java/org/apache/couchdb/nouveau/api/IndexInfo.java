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
public class IndexInfo {

    @PositiveOrZero
    private long updateSeq;

    @PositiveOrZero
    private int numDocs;

    @PositiveOrZero
    private long diskSize;

    public IndexInfo() {}

    public IndexInfo(final long updateSeq, final int numDocs, final long diskSize) {
        this.updateSeq = updateSeq;
        this.numDocs = numDocs;
        this.diskSize = diskSize;
    }

    @JsonProperty
    public int getNumDocs() {
        return numDocs;
    }

    @JsonProperty
    public long getDiskSize() {
        return diskSize;
    }

    @JsonProperty
    public long getUpdateSeq() {
        return updateSeq;
    }

    @Override
    public String toString() {
        return "IndexInfo [updateSeq=" + updateSeq + ", numDocs=" + numDocs + ", diskSize=" + diskSize + "]";
    }
}
