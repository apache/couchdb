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

package org.apache.couchdb.nouveau.core;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;

import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public abstract class BaseIndexTest<T> {

    protected abstract IndexLoader<T> indexLoader();

    protected abstract T stringField(final String name, final String value);

    protected final Index<T> setup(final Path path) throws IOException {
        final IndexDefinition indexDefinition = new IndexDefinition();
        indexDefinition.setDefaultAnalyzer("standard");
        final Index<T> index = indexLoader().apply(path, indexDefinition);
        index.setDeleteOnClose(true);
        return index;
    }

    protected final void cleanup(final Index<T> index) throws IOException {
        index.close();
    }

    @Test
    public void testOpenClose(@TempDir Path path) throws IOException {
        final Index<T> index = setup(path);
        cleanup(index);
    }

    @Test
    public void testSearching(@TempDir Path path) throws IOException {
        final Index<T> index = setup(path);
        try {
            final int count = 100;
            for (int i = 1; i <= count; i++) {
                final Collection<T> fields = List.of(stringField("foo", "bar"));
                final DocumentUpdateRequest<T> request = new DocumentUpdateRequest<T>(i, null, fields);
                index.update("doc" + i, request);
            }
            final SearchRequest request = new SearchRequest();
            request.setQuery("*:*");
            final SearchResults<T> results = index.search(request);
            assertThat(results.getTotalHits()).isEqualTo(count);
        } finally {
            cleanup(index);
        }
    }

}
