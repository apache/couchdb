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

package org.apache.couchdb.nouveau.tasks;

import io.dropwizard.servlets.tasks.Task;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.couchdb.nouveau.core.IndexManager;

public class SnapshotTask extends Task {

    private final IndexManager indexManager;

    public SnapshotTask(final IndexManager indexManager) {
        super("snapshot");
        this.indexManager = indexManager;
    }

    @Override
    public void execute(Map<String, List<String>> parameters, PrintWriter output) throws Exception {
        if (!parameters.containsKey("indexes")) {
            throw new IllegalArgumentException("'indexes' parameter is required");
        }
        for (final String name : parameters.get("indexes")) {
            output.format("%s\n", name);
            final Collection<String> files = indexManager.with(name, (index) -> {
                return index.snapshot();
            });
            for (final String file : files) {
                output.format("%s\n", file);
            }
        }
    }
}
