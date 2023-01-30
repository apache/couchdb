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

package org.apache.couchdb.nouveau.core.lucene9;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.core.Index;
import org.apache.couchdb.nouveau.core.Lucene;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.Analyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.TokenStream;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.couchdb.nouveau.lucene9.lucene.index.IndexWriter;
import org.apache.couchdb.nouveau.lucene9.lucene.index.IndexWriterConfig;
import org.apache.couchdb.nouveau.lucene9.lucene.misc.store.DirectIODirectory;
import org.apache.couchdb.nouveau.lucene9.lucene.search.SearcherFactory;
import org.apache.couchdb.nouveau.lucene9.lucene.search.SearcherManager;
import org.apache.couchdb.nouveau.lucene9.lucene.store.Directory;
import org.apache.couchdb.nouveau.lucene9.lucene.store.FSDirectory;

public class Lucene9 implements Lucene {

    private SearcherFactory searcherFactory;

    public void setExecutor(ExecutorService executor) {
        this.searcherFactory = new ParallelSearcherFactory(executor);
    }

    @Override
    public List<String> analyze(String analyzer, String text) throws IOException {
        try {
            return tokenize(Lucene9AnalyzerFactory.newAnalyzer(analyzer), text);
        } catch (IllegalArgumentException e) {
            throw new WebApplicationException(analyzer + " not a valid analyzer",
                    Status.BAD_REQUEST);
        }
    }

    private List<String> tokenize(final Analyzer analyzer, final String text) throws IOException {
        final List<String> result = new ArrayList<String>(10);
        try (final TokenStream tokenStream = analyzer.tokenStream("default", text)) {
            tokenStream.reset();
            while (tokenStream.incrementToken()) {
                final CharTermAttribute term = tokenStream.getAttribute(CharTermAttribute.class);
                result.add(term.toString());
            }
            tokenStream.end();
        }
        return result;
    }

    @Override
    public void validate(IndexDefinition indexDefinition) throws WebApplicationException {
        Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
    }

    @Override
    public Index open(Path path, IndexDefinition indexDefinition) throws IOException {
        final Analyzer analyzer = Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
        final Directory dir = new DirectIODirectory(FSDirectory.open(path));
        final IndexWriterConfig config = new IndexWriterConfig(analyzer);
        config.setUseCompoundFile(false);
        final IndexWriter writer = new IndexWriter(dir, config);
        final long updateSeq = getUpdateSeq(writer);
        final SearcherManager searcherManager = new SearcherManager(writer, searcherFactory);
        return new Lucene9Index(analyzer, writer, updateSeq, searcherManager);
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
