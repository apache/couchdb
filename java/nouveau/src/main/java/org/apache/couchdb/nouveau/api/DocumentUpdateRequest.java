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

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonProperty;

import org.apache.lucene.index.IndexableField;

import io.dropwizard.jackson.JsonSnakeCase;

@JsonSnakeCase
public class DocumentUpdateRequest {

    @Min(1)
    private long seq;

    private String partition;

    @NotEmpty
    private Collection<@NotNull IndexableField> fields;

    public DocumentUpdateRequest() {
        // Jackson deserialization
    }

    public DocumentUpdateRequest(long seq, String partition, Collection<IndexableField> fields) {
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
    public Collection<IndexableField> getFields() {
        return fields;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (int) (seq ^ (seq >>> 32));
        result = prime * result + ((partition == null) ? 0 : partition.hashCode());
        result = prime * result + ((fields == null) ? 0 : fields.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DocumentUpdateRequest other = (DocumentUpdateRequest) obj;
        if (seq != other.seq)
            return false;
        if (partition == null) {
            if (other.partition != null)
                return false;
        } else if (!partition.equals(other.partition))
            return false;
        if (fields == null) {
            if (other.fields != null)
                return false;
        } else if (!fields.equals(other.fields))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "DocumentUpdateRequest [seq=" + seq + ", partition=" + partition + ", fields=" + fields + "]";
    }

}
