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

import java.util.Collection;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.dropwizard.jackson.JsonSnakeCase;

@JsonSnakeCase
public class DocumentUpdateRequest<T> {

    @Min(1)
    private long seq;

    private String partition;

    @NotEmpty
    private Collection<T> fields;

    public DocumentUpdateRequest() {
        // Jackson deserialization
    }

    public DocumentUpdateRequest(long seq, String partition, Collection<T> fields) {
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
    public Collection<T> getFields() {
        return fields;
    }

    @Override
    public String toString() {
        return "DocumentUpdateRequest [seq=" + seq + ", partition=" + partition + ", fields=" + fields + "]";
    }

}
