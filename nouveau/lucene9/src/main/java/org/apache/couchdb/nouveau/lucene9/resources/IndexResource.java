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

package org.apache.couchdb.nouveau.lucene9.resources;

import java.io.IOException;
import java.util.List;
import java.util.Map;

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
import org.apache.couchdb.nouveau.core.IndexLoader;
import org.apache.couchdb.nouveau.core.IndexManager;
import org.apache.couchdb.nouveau.lucene9.core.Lucene9AnalyzerFactory;
import org.apache.couchdb.nouveau.lucene9.core.Lucene9Index;
import org.apache.couchdb.nouveau.resources.BaseIndexResource;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.misc.store.DirectIODirectory;
import org.apache.lucene.search.SearcherFactory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import com.codahale.metrics.annotation.ExceptionMetered;
import com.codahale.metrics.annotation.Metered;
import com.codahale.metrics.annotation.ResponseMetered;;

@Path("/9/index/{name}")
@Metered
@ResponseMetered
@ExceptionMetered(cause = IOException.class)
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class IndexResource extends BaseIndexResource<IndexableField> {

    private final SearcherFactory searcherFactory;

    public IndexResource(final IndexManager indexManager, final SearcherFactory searcherFactory) {
        super(indexManager);
        this.searcherFactory = searcherFactory;
    }

    @PUT
    @Override
    public void createIndex(@PathParam("name") String name, @NotNull @Valid IndexDefinition indexDefinition)
            throws IOException {
        super.createIndex(name, indexDefinition);
    }

    @DELETE
    @Path("/doc/{docId}")
    @Override
    public void deleteDoc(@PathParam("name") String name, @PathParam("docId") String docId,
            @NotNull @Valid DocumentDeleteRequest request) throws Exception {
        super.deleteDoc(name, docId, request);
    }

    @DELETE
    @Override
    public void deletePath(@PathParam("name") String path, @Valid final List<String> exclusions) throws IOException {
        super.deletePath(path, exclusions);
    }

    @GET
    @Override
    public IndexInfo indexInfo(@PathParam("name") String name) throws Exception {
        return super.indexInfo(name);
    }

    @POST
    @Path("/search")
    @Override
    public SearchResults<IndexableField> searchIndex(@PathParam("name") String name,
            @NotNull @Valid SearchRequest request)
            throws Exception {
        return super.searchIndex(name, request);
    }

    @PUT
    @Path("/doc/{docId}")
    @Override
    public void updateDoc(@PathParam("name") String name, @PathParam("docId") String docId,
            @NotNull @Valid DocumentUpdateRequest<IndexableField> request)
            throws Exception {
        super.updateDoc(name, docId, request);
    }

    @Override
    protected IndexLoader<IndexableField> indexLoader() {
        return (path, indexDefinition) -> {
            final Analyzer analyzer = Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
            final Directory dir = new DirectIODirectory(FSDirectory.open(path));
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
