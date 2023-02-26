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

import org.apache.lucene.index.IndexableField;
import org.apache.lucene.search.Query;

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.module.SimpleModule;

public class Lucene4Module extends SimpleModule {

    public Lucene4Module() {
        super("lucene4", Version.unknownVersion());

        // IndexableField
        addSerializer(IndexableField.class, new IndexableFieldSerializer());
        addDeserializer(IndexableField.class, new IndexableFieldDeserializer());

        // Query
        addDeserializer(Query.class, new QueryDeserializer());
    }

}
