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

package org.apache.couchdb.nouveau.lucene9.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.condition.OS.WINDOWS;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.DoubleRange;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.core.BaseIndexTest;
import org.apache.couchdb.nouveau.core.IndexLoader;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.DoubleDocValuesField;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.SortedDocValuesField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.misc.store.DirectIODirectory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;

public class Lucene9IndexTest extends BaseIndexTest<IndexableField> {

    @Override
    protected IndexLoader<IndexableField> indexLoader() {
        return (path, indexDefinition) -> {
            final Analyzer analyzer = Lucene9AnalyzerFactory.fromDefinition(indexDefinition);
            final Directory dir = new DirectIODirectory(FSDirectory.open(path));
            final IndexWriterConfig config = new IndexWriterConfig(analyzer);
            config.setUseCompoundFile(false);
            final IndexWriter writer = new IndexWriter(dir, config);
            final SearcherManager searcherManager = new SearcherManager(writer, null);
            return new Lucene9Index(analyzer, writer, 0L, searcherManager);
        };
    }

    protected IndexableField stringField(final String name, final String value) {
        return new StringField(name, value, Store.NO);
    }

    @Test
    @DisabledOnOs(WINDOWS)
    public void testCounts() throws IOException {
        final int count = 100;
        for (int i = 1; i <= count; i++) {
            final Collection<IndexableField> fields = List.of(new SortedDocValuesField("bar", new BytesRef("baz")));
            final DocumentUpdateRequest<IndexableField> request = new DocumentUpdateRequest<IndexableField>(i, null,
                    fields);
            index.update("doc" + i, request);
        }
        final SearchRequest request = new SearchRequest();
        request.setQuery("*:*");
        request.setCounts(List.of("bar"));
        final SearchResults<IndexableField> results = index.search(request);
        assertThat(results.getCounts()).isEqualTo(Map.of("bar", Map.of("baz", count)));
    }

    @Test
    @DisabledOnOs(WINDOWS)
    public void testRanges() throws IOException {
        final int count = 100;
        for (int i = 1; i <= count; i++) {
            final Collection<IndexableField> fields = List.of(new DoubleDocValuesField("bar", i));
            final DocumentUpdateRequest<IndexableField> request = new DocumentUpdateRequest<IndexableField>(i, null,
                    fields);
            index.update("doc" + i, request);
        }
        final SearchRequest request = new SearchRequest();
        request.setQuery("*:*");
        request.setRanges(Map.of("bar",
                List.of(new DoubleRange("low", 0.0, true, (double) count / 2, true),
                        new DoubleRange("high", (double) count / 2, true, (double) count, true))));
        final SearchResults<IndexableField> results = index.search(request);
        assertThat(results.getRanges()).isEqualTo(
                Map.of("bar", Map.of("low", count / 2, "high", count / 2 + 1)));
    }

}
