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
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.IndexInfo;
import org.apache.couchdb.nouveau.core.IndexManager;

import com.codahale.metrics.annotation.Timed;

@Path("/index/{name}")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class IndexResource {

    private final IndexManager indexManager;

    public IndexResource(final IndexManager indexManager) {
        this.indexManager = indexManager;
    }

    @GET
    @SuppressWarnings("resource")
    public IndexInfo indexInfo(@PathParam("name") String name) throws IOException {
        return indexManager.acquire(name).info();
    }

    @DELETE
    public void deletePath(@PathParam("name") String path) throws IOException {
        indexManager.deleteAll(path);
    }

    @PUT
    public void createIndex(@PathParam("name") String name, @NotNull @Valid IndexDefinition indexDefinition) throws IOException {
        indexManager.create(name, indexDefinition);
    }

    @DELETE
    @Timed
    @Path("/doc/{docId}")
    public void deleteDoc(@PathParam("name") String name, @PathParam("docId") String docId, @NotNull @Valid final DocumentDeleteRequest request) throws IOException {
        indexManager.acquire(name).delete(docId, request);
    }

    @PUT
    @Timed
    @Path("/doc/{docId}")
    public void updateDoc(@PathParam("name") String name, @PathParam("docId") String docId, @NotNull @Valid final DocumentUpdateRequest request) throws IOException {
        indexManager.acquire(name).update(docId, request);
    }

}