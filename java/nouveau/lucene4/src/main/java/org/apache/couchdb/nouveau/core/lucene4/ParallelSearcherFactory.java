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
import java.util.concurrent.ExecutorService;

import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.SearcherFactory;

class ParallelSearcherFactory extends SearcherFactory {

    private ExecutorService executorService;

    public ParallelSearcherFactory(ExecutorService executorService) {
        this.executorService = executorService;
    }

    @Override
    public IndexSearcher newSearcher(final IndexReader reader) throws IOException {
        return new IndexSearcher(reader, executorService);
    }

}
