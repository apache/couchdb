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

import java.io.IOException;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.apache.couchdb.nouveau.api.AnalyzeRequest;
import org.apache.couchdb.nouveau.api.AnalyzeResponse;
import org.apache.couchdb.nouveau.core.Lucene;

import com.codahale.metrics.annotation.Timed;

@Path("/analyze")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class AnalyzeResource {

    private final Lucene lucene;

    public AnalyzeResource(Lucene lucene) {
        this.lucene = lucene;
    }

    @POST
    @Timed
    public AnalyzeResponse analyzeText(@NotNull @Valid AnalyzeRequest analyzeRequest) throws IOException {
        final List<String> tokens = lucene.analyze(analyzeRequest.getAnalyzer(), analyzeRequest.getText());
        return new AnalyzeResponse(tokens);
    }

}
