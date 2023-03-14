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

package org.apache.couchdb.nouveau.lucene9.health;

import java.util.Collections;

import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.health.BaseIndexHealthCheck;
import org.apache.couchdb.nouveau.lucene9.resources.IndexResource;
import org.apache.lucene.index.IndexableField;

public class IndexHealthCheck extends BaseIndexHealthCheck<IndexableField> {

    public IndexHealthCheck(IndexResource indexResource) {
        super(indexResource);
    }

    @Override
    protected String generateIndexName() {
        return "___test9";
    }

    @Override
    protected DocumentUpdateRequest<IndexableField>  generateDocumentUpdateRequest() {
        return new DocumentUpdateRequest<IndexableField>(1, null, Collections.emptyList());
    }

    @Override
    protected int getLuceneMajor() {
        return 9;
    }

}
