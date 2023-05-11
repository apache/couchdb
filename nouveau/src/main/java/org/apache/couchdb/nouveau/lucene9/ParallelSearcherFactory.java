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

package org.apache.couchdb.nouveau.lucene9;

import java.io.IOException;
import java.util.concurrent.Executor;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.SearcherFactory;

public class ParallelSearcherFactory extends SearcherFactory {

    private Executor executor;

    public ParallelSearcherFactory(Executor executor) {
        this.executor = executor;
    }

    @Override
    public IndexSearcher newSearcher(final IndexReader reader, final IndexReader previousReader) throws IOException {
        return new IndexSearcher(reader, executor);
    }
}
