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

import com.codahale.metrics.annotation.ExceptionMetered;
import com.codahale.metrics.annotation.Metered;
import com.codahale.metrics.annotation.ResponseMetered;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.IndexInfo;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.core.IndexLoader;
import org.apache.couchdb.nouveau.core.IndexManager;
import org.apache.couchdb.nouveau.lucene9.Lucene9AnalyzerFactory;
import org.apache.couchdb.nouveau.lucene9.Lucene9Index;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.misc.store.DirectIODirectory;
import org.apache.lucene.search.SearcherFactory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

@Path("/index/{name}")
@Metered
@ResponseMetered
@ExceptionMetered(cause = IOException.class)
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public final class IndexResource {

    private final IndexManager indexManager;
    private final SearcherFactory searcherFactory;

    public IndexResource(final IndexManager indexManager, final SearcherFactory searcherFactory) {
        this.indexManager = Objects.requireNonNull(indexManager);
        this.searcherFactory = Objects.requireNonNull(searcherFactory);
    }

    @PUT
    public void createIndex(@PathParam("name") String name, @NotNull @Valid IndexDefinition indexDefinition)
            throws IOException {
        indexManager.create(name, indexDefinition);
    }

    @DELETE
    @Path("/doc/{docId}")
    public void deleteDoc(
            @PathParam("name") String name,
            @PathParam("docId") String docId,
            @NotNull @Valid DocumentDeleteRequest request)
            throws Exception {
        indexManager.with(name, indexLoader(), (index) -> {
            index.delete(docId, request);
            return null;
        });
    }

    @DELETE
    public void deletePath(@PathParam("name") String path, @Valid final List<String> exclusions) throws IOException {
        indexManager.deleteAll(path, exclusions);
    }

    @GET
    public IndexInfo indexInfo(@PathParam("name") String name) throws Exception {
        return indexManager.with(name, indexLoader(), (index) -> {
            return index.info();
        });
    }

    @POST
    @Path("/search")
    public SearchResults searchIndex(@PathParam("name") String name, @NotNull @Valid SearchRequest request)
            throws Exception {
        return indexManager.with(name, indexLoader(), (index) -> {
            return index.search(request);
        });
    }

    @PUT
    @Path("/doc/{docId}")
    public void updateDoc(
            @PathParam("name") String name,
            @PathParam("docId") String docId,
            @NotNull @Valid DocumentUpdateRequest request)
            throws Exception {
        indexManager.with(name, indexLoader(), (index) -> {
            index.update(docId, request);
            return null;
        });
    }

    private IndexLoader indexLoader() {
        return (path, indexDefinition) -> {
            final Analyzer analyzer = Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
            final Directory dir = new DirectIODirectory(FSDirectory.open(path.resolve("9")));
            final IndexWriterConfig config = new IndexWriterConfig(analyzer);
            config.setUseCompoundFile(false);
            final IndexWriter writer = new IndexWriter(dir, config);
            final long updateSeq = getUpdateSeq(writer);
            final SearcherManager searcherManager = new SearcherManager(writer, searcherFactory);
            return new Lucene9Index(analyzer, writer, updateSeq, searcherManager);
        };
    }

    private static long getUpdateSeq(final IndexWriter writer) throws IOException {
        final Iterable<Map.Entry<String, String>> commitData = writer.getLiveCommitData();
        if (commitData == null) {
            return 0L;
        }
        for (Map.Entry<String, String> entry : commitData) {
            if (entry.getKey().equals("update_seq")) {
                return Long.parseLong(entry.getValue());
            }
        }
        return 0L;
    }
}
