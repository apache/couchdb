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
import java.util.Map;

import javax.ws.rs.Path;

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
import org.apache.lucene.store.FSDirectory;;

@Path("/9/index/{name}")
public class IndexResource extends BaseIndexResource<IndexableField> {

    private final SearcherFactory searcherFactory;

    public IndexResource(final IndexManager indexManager, final SearcherFactory searcherFactory) {
        super(indexManager);
        this.searcherFactory = searcherFactory;
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