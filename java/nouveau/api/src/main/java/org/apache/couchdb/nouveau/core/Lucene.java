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

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import javax.ws.rs.WebApplicationException;

import org.apache.couchdb.nouveau.api.IndexDefinition;

public interface Lucene {

    int getMajor();

    List<String> analyze(final String analyzer, final String text) throws IOException;

    void validate(final IndexDefinition indexDefinition) throws WebApplicationException;

    Index open(final Path path, final IndexDefinition indexDefinition) throws IOException;

}
