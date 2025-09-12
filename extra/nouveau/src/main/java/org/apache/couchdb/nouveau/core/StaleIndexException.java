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

package org.apache.couchdb.nouveau.core;

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response.Status;

public final class StaleIndexException extends WebApplicationException {

    public StaleIndexException(final boolean purge, final long minSeq, final long actualSeq) {
        super(
                String.format(
                        "index is stale (%s seq needs to be at least %d but is %d)",
                        purge ? "purge" : "index", minSeq, actualSeq),
                Status.CONFLICT);
    }
}
