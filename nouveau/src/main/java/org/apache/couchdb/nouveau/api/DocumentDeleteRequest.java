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

public final class DocumentDeleteRequest {

    @Positive
    private final long seq;

    private final boolean purge;

    public DocumentDeleteRequest(@JsonProperty("seq") final long seq, @JsonProperty("purge") final boolean purge) {
        if (seq < 1) {
            throw new IllegalArgumentException("seq must be 1 or greater");
        }
        this.seq = seq;
        this.purge = purge;
    }

    public long getSeq() {
        return seq;
    }

    public boolean isPurge() {
        return purge;
    }

    @Override
    public String toString() {
        return "DocumentDeleteRequest [seq=" + seq + ", purge=" + purge + "]";
    }
}
