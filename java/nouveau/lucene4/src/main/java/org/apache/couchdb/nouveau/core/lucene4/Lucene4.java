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

package org.apache.couchdb.nouveau.core.lucene4;

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
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.search.SearcherFactory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

final class Lucene4 implements Lucene {

    private SearcherFactory searcherFactory;

    public int getMajor() {
        return 4;
    }

    public void setExecutor(ExecutorService executor) {
        this.searcherFactory = new ParallelSearcherFactory(executor);
    }

    @Override
    public List<String> analyze(String analyzer, String text) throws IOException {
        try {
            return tokenize(Lucene4AnalyzerFactory.newAnalyzer(analyzer), text);
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
        Lucene4AnalyzerFactory.fromDefinition(indexDefinition);
    }

    @Override
    public Index open(Path path, IndexDefinition indexDefinition) throws IOException {
        final Analyzer analyzer = Lucene4AnalyzerFactory.fromDefinition(indexDefinition);
        final Directory dir = FSDirectory.open(path.toFile());
        final IndexWriterConfig config = new IndexWriterConfig(Utils.LUCENE_VERSION, analyzer);
        config.setUseCompoundFile(false);
        final IndexWriter writer = new IndexWriter(dir, config);
        final long updateSeq = getUpdateSeq(writer);
        final SearcherManager searcherManager = new SearcherManager(writer, true, searcherFactory);
        return new Lucene9Index(analyzer, writer, updateSeq, searcherManager);
    }

    private static long getUpdateSeq(final IndexWriter writer) throws IOException {
        final Map<String, String> commitData = writer.getCommitData();
        if (commitData == null) {
            return 0L;
        }
        return Long.parseLong(commitData.getOrDefault("update_seq", "0"));
    }

}
