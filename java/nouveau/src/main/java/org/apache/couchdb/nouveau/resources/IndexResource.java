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
import org.apache.couchdb.nouveau.core.DocumentFactory;
import org.apache.couchdb.nouveau.core.IndexManager;
import org.apache.couchdb.nouveau.core.IndexManager.Index;
import com.codahale.metrics.annotation.Timed;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.TermQuery;

@Path("/index/{name}")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class IndexResource {

    private final IndexManager indexManager;
    private final DocumentFactory documentFactory;

    public IndexResource(final IndexManager indexManager, final DocumentFactory documentFactory) {
        this.indexManager = indexManager;
        this.documentFactory = documentFactory;
    }

    @GET
    @SuppressWarnings("resource")
    public IndexInfo indexInfo(@PathParam("name") String name) throws IOException {
        final long updateSeq;
        final int numDocs;
        final Index index = indexManager.acquire(name);
        try {
            updateSeq = index.getUpdateSeq();
            numDocs = index.getWriter().getDocStats().numDocs;
        } finally {
            indexManager.release(index);
        }
        return new IndexInfo(updateSeq, numDocs);
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
        final Index index = indexManager.acquire(name);
        try {
            final IndexWriter writer = index.getWriter();
            writer.deleteDocuments(new TermQuery(new Term("_id", docId)));
            index.incrementUpdateSeq(request.getSeq());
        } finally {
            indexManager.release(index);
        }
    }

    @PUT
    @Timed
    @Path("/doc/{docId}")
    public void updateDoc(@PathParam("name") String name, @PathParam("docId") String docId, @NotNull @Valid final DocumentUpdateRequest request) throws IOException {
        final Index index = indexManager.acquire(name);
        try {
            final IndexWriter writer = index.getWriter();
            final Document doc = documentFactory.build(docId, request);
            writer.updateDocument(new Term("_id", docId), doc);
            index.incrementUpdateSeq(request.getSeq());
        } finally {
            indexManager.release(index);
        }
    }

}