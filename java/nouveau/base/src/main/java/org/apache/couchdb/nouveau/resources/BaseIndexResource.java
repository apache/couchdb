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

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.IndexInfo;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.core.Index;
import org.apache.couchdb.nouveau.core.IndexLoader;
import org.apache.couchdb.nouveau.core.IndexManager;

import com.codahale.metrics.annotation.Timed;

@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public abstract class BaseIndexResource<T> {

    protected final IndexManager indexManager;

    protected BaseIndexResource(final IndexManager indexManager) {
        this.indexManager = indexManager;
    }

    @GET
    @SuppressWarnings("unchecked")
    public final IndexInfo indexInfo(@PathParam("name") String name)
            throws Exception {
        final Index<T> index = indexManager.acquire(name, indexLoader());
        try {
            return index.info();
        } finally {
            indexManager.release(name, index);
        }
    }

    @DELETE
    public final void deletePath(@PathParam("name") String path) throws IOException {
        indexManager.deleteAll(path);
    }

    @PUT
    public final void createIndex(@PathParam("name") String name, @NotNull @Valid IndexDefinition indexDefinition)
            throws IOException {
        indexManager.create(name, indexDefinition);
    }

    @DELETE
    @Timed
    @Path("/doc/{docId}")
    @SuppressWarnings("unchecked")
    public final void deleteDoc(@PathParam("name") String name, @PathParam("docId") String docId,
            @NotNull @Valid final DocumentDeleteRequest request)
            throws Exception {
        final Index<T> index = indexManager.acquire(name, indexLoader());
        try {
            index.delete(docId, request);
        } finally {
            indexManager.release(name, index);
        }
    }

    @PUT
    @Timed
    @Path("/doc/{docId}")
    @SuppressWarnings("unchecked")
    public final void updateDoc(@PathParam("name") String name, @PathParam("docId") String docId,
            @NotNull @Valid final DocumentUpdateRequest<T> request)
            throws Exception {
        final Index<T> index = indexManager.acquire(name, indexLoader());
        try {
            index.update(docId, request);
        } finally {
            indexManager.release(name, index);
        }
    }

    @POST
    @Timed
    @Path("/search")
    @SuppressWarnings("unchecked")
    public final SearchResults<T> searchIndex(@PathParam("name") String name, @NotNull @Valid SearchRequest request)
            throws Exception {
        final Index<T> index = indexManager.acquire(name, indexLoader());
        try {
            return index.search(request);
        } finally {
            indexManager.release(name, index);
        }
    }

    protected abstract IndexLoader<T> indexLoader();

}