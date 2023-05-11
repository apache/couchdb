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
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Positive;
import java.util.Collection;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class DocumentUpdateRequest {

    @Positive
    private long seq;

    private String partition;

    @NotEmpty
    @Valid
    private Collection<Field> fields;

    public DocumentUpdateRequest() {
        // Jackson deserialization
    }

    public DocumentUpdateRequest(long seq, String partition, Collection<Field> fields) {
        this.seq = seq;
        this.partition = partition;
        this.fields = fields;
    }

    @JsonProperty
    public long getSeq() {
        return seq;
    }

    @JsonProperty
    public String getPartition() {
        return partition;
    }

    public boolean hasPartition() {
        return partition != null;
    }

    @JsonProperty
    public Collection<Field> getFields() {
        return fields;
    }

    @Override
    public String toString() {
        return "DocumentUpdateRequest [seq=" + seq + ", partition=" + partition + ", fields=" + fields + "]";
    }
}
