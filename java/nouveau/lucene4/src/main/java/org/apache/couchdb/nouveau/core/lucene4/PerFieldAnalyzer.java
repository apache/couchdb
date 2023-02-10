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

import java.util.Map;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.AnalyzerWrapper;

// This only exists because PerFieldAnalyzerWrapper#getWrappedAnalyzer is protected
public class PerFieldAnalyzer extends AnalyzerWrapper {

    private final Analyzer defaultAnalyzer;
    private final Map<String, Analyzer> fieldAnalyzers;

    public PerFieldAnalyzer(Analyzer defaultAnalyzer,
            Map<String, Analyzer> fieldAnalyzers) {
        super(Analyzer.PER_FIELD_REUSE_STRATEGY);
        this.defaultAnalyzer = defaultAnalyzer;
        this.fieldAnalyzers = fieldAnalyzers;
    }

    @Override
    protected Analyzer getWrappedAnalyzer(String fieldName) {
        return fieldAnalyzers.getOrDefault(fieldName, defaultAnalyzer);
    }

}
