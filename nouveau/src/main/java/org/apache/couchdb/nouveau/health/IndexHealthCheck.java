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

package org.apache.couchdb.nouveau.health;

import com.codahale.metrics.health.HealthCheck;
import java.io.IOException;
import java.util.Collections;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.resources.IndexResource;

public final class IndexHealthCheck extends HealthCheck {

    private final IndexResource indexResource;

    public IndexHealthCheck(final IndexResource indexResource) {
        this.indexResource = indexResource;
    }

    @Override
    protected Result check() throws Exception {
        final String name = "___test9";
        try {
            indexResource.deletePath(name, null);
        } catch (IOException e) {
            // Ignored, index might not exist yet.
        }

        indexResource.createIndex(name, new IndexDefinition("standard", null));
        try {
            final DocumentUpdateRequest documentUpdateRequest =
                    new DocumentUpdateRequest(0, 1, null, Collections.emptyList());
            indexResource.updateDoc(name, "foo", documentUpdateRequest);

            final SearchRequest searchRequest = new SearchRequest();
            searchRequest.setQuery("_id:foo");

            final SearchResults searchResults = indexResource.searchIndex(name, searchRequest);
            if (searchResults.getTotalHits() == 1) {
                return Result.healthy();
            } else {
                return Result.unhealthy(
                        "Wrong number of search results, expected 1, got %d", searchResults.getTotalHits());
            }
        } finally {
            indexResource.deletePath(name, null);
        }
    }
}
