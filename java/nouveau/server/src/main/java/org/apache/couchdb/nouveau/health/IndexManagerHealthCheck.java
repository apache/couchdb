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

package org.apache.couchdb.nouveau.health;

import static org.apache.couchdb.nouveau.api.LuceneVersion.LUCENE_9;

import java.io.IOException;
import java.util.Collections;

import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.core.Index;
import org.apache.couchdb.nouveau.core.IndexManager;

import com.codahale.metrics.health.HealthCheck;

public class IndexManagerHealthCheck extends HealthCheck {

    private IndexManager indexManager;

    public IndexManagerHealthCheck(final IndexManager indexManager) {
        this.indexManager = indexManager;
    }

    @Override
    protected Result check() throws Exception {
        final String name = "_____test";
        try {
            indexManager.deleteAll(name);
        } catch (IOException e) {
            // Ignored, index might not exist yet.
        }

        indexManager.create(name, new IndexDefinition(LUCENE_9, "standard", null));
        final Index index = indexManager.acquire(name);
        final DocumentUpdateRequest request = new DocumentUpdateRequest(1, null, Collections.emptyList());
        index.update("foo", request);
        index.commit();
        index.setDeleteOnClose(true);
        indexManager.invalidate(name);
        return Result.healthy();
    }

}
