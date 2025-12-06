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

package org.apache.couchdb.nouveau.resources;

import com.codahale.metrics.annotation.ExceptionMetered;
import com.codahale.metrics.annotation.Metered;
import com.codahale.metrics.annotation.ResponseMetered;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response.Status;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.couchdb.nouveau.api.AnalyzeRequest;
import org.apache.couchdb.nouveau.api.AnalyzeResponse;
import org.apache.couchdb.nouveau.lucene9.Lucene9AnalyzerFactory;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;

@Path("/analyze")
@Metered
@ResponseMetered
@ExceptionMetered(cause = IOException.class)
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public final class AnalyzeResource {

    @POST
    public AnalyzeResponse analyzeText(@NotNull @Valid AnalyzeRequest request) throws IOException {
        try {
            final List<String> tokens =
                    tokenize(Lucene9AnalyzerFactory.newAnalyzer(request.getAnalyzer()), request.getText());
            return new AnalyzeResponse(tokens);
        } catch (IllegalArgumentException e) {
            throw new WebApplicationException(request.getAnalyzer() + " not a valid analyzer", Status.BAD_REQUEST);
        }
    }

    private List<String> tokenize(final Analyzer analyzer, final String text) throws IOException {
        final List<String> result = new ArrayList<String>(10);
        try (final TokenStream tokenStream = analyzer.tokenStream("default", text)) {
            tokenStream.reset();
            while (tokenStream.incrementToken()) {
                final CharTermAttribute term = tokenStream.getAttribute(CharTermAttribute.class);
                result.add(term.toString());
            }
            tokenStream.end();
        }
        return result;
    }
}
