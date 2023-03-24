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

import java.io.Reader;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.LetterTokenizer;
import org.apache.lucene.analysis.core.LowerCaseFilter;
import org.apache.lucene.analysis.miscellaneous.ASCIIFoldingFilter;
import org.apache.lucene.util.Version;

class SimpleAsciiFoldingAnalyzer extends Analyzer {

    private Version version;

    SimpleAsciiFoldingAnalyzer(final Version version) {
        this.version = version;
    }

    @Override
    protected TokenStreamComponents createComponents(String fieldName, Reader reader) {
        var tokenizer = new LetterTokenizer(version, reader);
        return new TokenStreamComponents(tokenizer, new ASCIIFoldingFilter(new LowerCaseFilter(version, tokenizer)));
    }

}
