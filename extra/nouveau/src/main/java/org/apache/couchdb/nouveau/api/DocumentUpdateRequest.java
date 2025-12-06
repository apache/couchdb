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
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import java.util.Collection;

public final class DocumentUpdateRequest {

    @PositiveOrZero
    private final long matchSeq;

    @Positive
    private final long seq;

    private final String partition;

    @NotEmpty
    @Valid
    private final Collection<Field> fields;

    public DocumentUpdateRequest(
            @JsonProperty("match_seq") final long matchSeq,
            @JsonProperty("seq") final long seq,
            @JsonProperty("partition") final String partition,
            @JsonProperty("fields") final Collection<Field> fields) {
        this.matchSeq = matchSeq;
        this.seq = seq;
        this.partition = partition;
        this.fields = fields;
    }

    public long getMatchSeq() {
        return matchSeq;
    }

    public long getSeq() {
        return seq;
    }

    public String getPartition() {
        return partition;
    }

    public boolean hasPartition() {
        return partition != null;
    }

    public Collection<Field> getFields() {
        return fields;
    }

    @Override
    public String toString() {
        return "DocumentUpdateRequest [matchSeq=" + matchSeq + ", seq=" + seq + ", partition=" + partition + ", fields="
                + fields + "]";
    }
}
