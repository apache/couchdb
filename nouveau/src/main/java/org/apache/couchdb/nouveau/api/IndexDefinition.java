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

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import java.util.Map;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class IndexDefinition {

    public static final int LEGACY_LUCENE_VERSION = 9;
    public static final int LATEST_LUCENE_VERSION = 10;

    @Min(LEGACY_LUCENE_VERSION)
    @Max(LATEST_LUCENE_VERSION)
    private int luceneVersion = LEGACY_LUCENE_VERSION; // Legacy version if not set.

    @NotEmpty
    private String defaultAnalyzer;

    private Map<@NotEmpty String, @NotEmpty String> fieldAnalyzers;

    public IndexDefinition() {
        // Jackson deserialization
    }

    public IndexDefinition(
            final int luceneVersion, final String defaultAnalyzer, final Map<String, String> fieldAnalyzers) {
        this.luceneVersion = luceneVersion;
        this.defaultAnalyzer = defaultAnalyzer;
        this.fieldAnalyzers = fieldAnalyzers;
    }

    @JsonProperty
    public int getLuceneVersion() {
        return luceneVersion;
    }

    public void setLuceneVersion(int luceneVersion) {
        this.luceneVersion = luceneVersion;
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
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + luceneVersion;
        result = prime * result + ((defaultAnalyzer == null) ? 0 : defaultAnalyzer.hashCode());
        result = prime * result + ((fieldAnalyzers == null) ? 0 : fieldAnalyzers.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        IndexDefinition other = (IndexDefinition) obj;
        if (luceneVersion != other.luceneVersion) return false;
        if (defaultAnalyzer == null) {
            if (other.defaultAnalyzer != null) return false;
        } else if (!defaultAnalyzer.equals(other.defaultAnalyzer)) return false;
        if (fieldAnalyzers == null) {
            if (other.fieldAnalyzers != null) return false;
        } else if (!fieldAnalyzers.equals(other.fieldAnalyzers)) return false;
        return true;
    }

    @Override
    public String toString() {
        return "IndexDefinition [luceneVersion=" + luceneVersion + ", defaultAnalyzer=" + defaultAnalyzer
                + ", fieldAnalyzers=" + fieldAnalyzers + "]";
    }
}
