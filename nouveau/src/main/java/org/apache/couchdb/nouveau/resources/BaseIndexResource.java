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

package org.apache.couchdb.nouveau.resources;

import java.io.IOException;
import java.util.List;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.IndexInfo;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.core.IndexLoader;
import org.apache.couchdb.nouveau.core.IndexManager;

public abstract class BaseIndexResource<T> {

    protected final IndexManager indexManager;

    protected BaseIndexResource(final IndexManager indexManager) {
        this.indexManager = indexManager;
    }

    public IndexInfo indexInfo(String name)
            throws Exception {
        return indexManager.with(name, indexLoader(), (index) -> {
            return index.info();
        });
    }

    public void deletePath(String path, @Valid List<String> exclusions) throws IOException {
        indexManager.deleteAll(path, exclusions);
    }

    public void createIndex(String name, @NotNull @Valid IndexDefinition indexDefinition)
            throws IOException {
        indexManager.create(name, indexDefinition);
    }

    public void deleteDoc(String name, String docId,
            @NotNull @Valid final DocumentDeleteRequest request)
            throws Exception {
        indexManager.with(name, indexLoader(), (index) -> {
            index.delete(docId, request);
            return null;
        });
    }

    @SuppressWarnings("unchecked")
    public void updateDoc(String name, String docId,
            @NotNull @Valid final DocumentUpdateRequest<T> request)
            throws Exception {
        indexManager.with(name, indexLoader(), (index) -> {
            index.update(docId, request);
            return null;
        });
    }

    @SuppressWarnings("unchecked")
    public SearchResults<T> searchIndex(String name, @NotNull @Valid SearchRequest request)
            throws Exception {
        return indexManager.with(name, indexLoader(), (index) -> {
            return index.search(request);
        });
    }

    protected abstract IndexLoader<T> indexLoader();

}
