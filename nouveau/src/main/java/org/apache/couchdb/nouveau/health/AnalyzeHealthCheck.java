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

import com.codahale.metrics.health.HealthCheck;
import java.util.Arrays;
import java.util.List;
import org.apache.couchdb.nouveau.api.AnalyzeRequest;
import org.apache.couchdb.nouveau.api.AnalyzeResponse;
import org.apache.couchdb.nouveau.resources.AnalyzeResource;

public final class AnalyzeHealthCheck extends HealthCheck {

    private AnalyzeResource analyzeResource;

    public AnalyzeHealthCheck(final AnalyzeResource analyzeResource) {
        this.analyzeResource = analyzeResource;
    }

    @Override
    protected Result check() throws Exception {
        final AnalyzeRequest request = new AnalyzeRequest("standard", "hello goodbye");
        final AnalyzeResponse response = analyzeResource.analyzeText(request);
        final List<String> expected = Arrays.asList("hello", "goodbye");
        final List<String> actual = response.getTokens();
        if (expected.equals(actual)) {
            return Result.healthy();
        } else {
            return Result.unhealthy("Expected '%s' but got '%s'", expected, actual);
        }
    }
}
