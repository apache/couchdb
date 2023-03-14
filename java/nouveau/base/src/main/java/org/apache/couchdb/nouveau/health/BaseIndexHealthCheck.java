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

import java.io.IOException;

import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.resources.BaseIndexResource;

import com.codahale.metrics.health.HealthCheck;

public abstract class BaseIndexHealthCheck<T> extends HealthCheck {

    private final BaseIndexResource<T> indexResource;

    protected BaseIndexHealthCheck(final BaseIndexResource<T> indexResource) {
        this.indexResource = indexResource;
    }

    protected abstract String generateIndexName();

    protected abstract DocumentUpdateRequest<T> generateDocumentUpdateRequest();

    protected abstract int getLuceneMajor();

    @Override
    protected Result check() throws Exception {
        final String name = generateIndexName();
        try {
            indexResource.deletePath(name);
        } catch (IOException e) {
            // Ignored, index might not exist yet.
        }

        indexResource.createIndex(name, new IndexDefinition(getLuceneMajor(), "standard", null));
        try {
            final DocumentUpdateRequest<T> documentUpdateRequest = generateDocumentUpdateRequest();
            indexResource.updateDoc(name, "foo", documentUpdateRequest);

            final SearchRequest searchRequest = new SearchRequest();
            searchRequest.setQuery("_id:foo");

            final SearchResults<T> searchResults = indexResource.searchIndex(name, searchRequest);
            if (searchResults.getTotalHits() == 1) {
                return Result.healthy();
            }
        } finally {
            indexResource.deletePath(name);
        }
        return Result.unhealthy(name);
    }

}
