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

package org.apache.couchdb.nouveau.lucene4.core;

import org.apache.couchdb.nouveau.core.BaseIndexTest;
import org.apache.couchdb.nouveau.core.IndexLoader;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

public class Lucene4IndexTest extends BaseIndexTest<IndexableField> {

    @Override
    protected IndexLoader<IndexableField> indexLoader() {
        return (path, indexDefinition) -> {
            final Analyzer analyzer = Lucene4AnalyzerFactory.fromDefinition(indexDefinition);
            final Directory dir = FSDirectory.open(path.toFile());
            final IndexWriterConfig config = new IndexWriterConfig(Utils.LUCENE_VERSION, analyzer);
            config.setUseCompoundFile(false);
            final IndexWriter writer = new IndexWriter(dir, config);
            final SearcherManager searcherManager = new SearcherManager(writer, true, null);
            return new Lucene4Index(analyzer, writer, 0L, searcherManager);
        };
    }

}
