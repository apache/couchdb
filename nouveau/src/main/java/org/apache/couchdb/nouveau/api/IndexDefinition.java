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

package org.apache.couchdb.nouveau.api;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;

import io.dropwizard.jackson.JsonSnakeCase;
import jakarta.validation.constraints.NotEmpty;

@JsonSnakeCase
@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class IndexDefinition {

    @NotEmpty
    private String defaultAnalyzer;

    private Map<@NotEmpty String, @NotEmpty String> fieldAnalyzers;

    public IndexDefinition() {
        // Jackson deserialization
    }

    public IndexDefinition(final String defaultAnalyzer, final Map<String, String> fieldAnalyzers) {
        this.defaultAnalyzer = defaultAnalyzer;
        this.fieldAnalyzers = fieldAnalyzers;
    }

    @JsonProperty
    public String getDefaultAnalyzer() {
        return defaultAnalyzer;
    }

    public void setDefaultAnalyzer(String defaultAnalyzer) {
        this.defaultAnalyzer = defaultAnalyzer;
    }

    @JsonProperty
    public Map<String, String> getFieldAnalyzers() {
        return fieldAnalyzers;
    }

    public void setFieldAnalyzers(Map<String, String> fieldAnalyzers) {
        this.fieldAnalyzers = fieldAnalyzers;
    }

    public boolean hasFieldAnalyzers() {
        return fieldAnalyzers != null && !fieldAnalyzers.isEmpty();
    }

    @Override
    public String toString() {
        return "IndexDefinition [defaultAnalyzer=" + defaultAnalyzer
                + ", fieldAnalyzers=" + fieldAnalyzers + "]";
    }

}
