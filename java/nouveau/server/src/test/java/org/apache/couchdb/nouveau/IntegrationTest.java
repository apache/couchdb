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

package org.apache.couchdb.nouveau;

import static org.apache.couchdb.nouveau.api.LuceneVersion.LUCENE_9;
import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.lucene.document.DoubleDocValuesField;
import org.apache.lucene.document.DoublePoint;
import org.apache.lucene.document.SortedSetDocValuesField;
import org.apache.lucene.facet.range.DoubleRange;
import org.apache.lucene.search.FieldDoc;
import org.apache.lucene.util.BytesRef;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.dropwizard.configuration.ResourceConfigurationSourceProvider;
import io.dropwizard.testing.junit5.DropwizardAppExtension;
import io.dropwizard.testing.junit5.DropwizardExtensionsSupport;

@ExtendWith(DropwizardExtensionsSupport.class)
public class IntegrationTest {

    private static final String CONFIG = "test-nouveau.yaml";

    static final DropwizardAppExtension<NouveauApplicationConfiguration> APP = new DropwizardAppExtension<>(
            NouveauApplication.class, CONFIG,
            new ResourceConfigurationSourceProvider()
    );

    @Test
    public void indexTest() throws Exception{
        final String url = "http://localhost:" + APP.getLocalPort();
        final String indexName = "foo";
        final IndexDefinition indexDefinition = new IndexDefinition(LUCENE_9, "standard", null);

        // Clean up.
        Response response =
                APP.client().target(String.format("%s/index/%s", url, indexName))
                .request()
                .delete();

        // Create index.
        response =
                APP.client().target(String.format("%s/index/%s", url, indexName))
                .request()
                .put(Entity.entity(indexDefinition, MediaType.APPLICATION_JSON_TYPE));

        assertThat(response).extracting(Response::getStatus)
        .isEqualTo(Response.Status.NO_CONTENT.getStatusCode());

        // Populate index
        for (int i = 0; i < 10; i++) {
            final DocumentUpdateRequest docUpdate = new DocumentUpdateRequest(i + 1, null,
                List.of(
                    new DoublePoint("foo", i), 
                    new DoubleDocValuesField("baz", i),
                    new SortedSetDocValuesField("bar", new BytesRef("baz"))));
            response = 
                APP.client().target(String.format("%s/index/%s/doc/doc%d", url, indexName, i))
                .request()
                .put(Entity.entity(docUpdate, MediaType.APPLICATION_JSON_TYPE));
            assertThat(response).extracting(Response::getStatus)
            .isEqualTo(Response.Status.NO_CONTENT.getStatusCode());
        }

        // Search index
        final SearchRequest searchRequest = new SearchRequest();
        searchRequest.setQuery("*:*");
        searchRequest.setLimit(10);
        searchRequest.setCounts(List.of("bar"));
        searchRequest.setRanges(Map.of("baz", List.of(new DoubleRange("0 to 100 inc", 0.0, true, 100.0, true))));
        searchRequest.setTopN(2);
        searchRequest.setAfter(new FieldDoc(0, Float.NaN, new Object[]{1.0f, new BytesRef("a")}));

        response =
                APP.client().target(String.format("%s/index/%s/search", url, indexName))
                .request()
                .post(Entity.entity(searchRequest, MediaType.APPLICATION_JSON_TYPE));

        assertThat(response).extracting(Response::getStatus).isEqualTo(Response.Status.OK.getStatusCode());
        final SearchResults results = response.readEntity(SearchResults.class);
        assertThat(results.getTotalHits()).isEqualTo(10);
        assertThat(results.getTotalHitsRelation()).isEqualTo("EQUAL_TO");
        assertThat(results.getCounts().size()).isEqualTo(1);
        assertThat(results.getCounts().get("bar").get("baz")).isEqualTo(10);
        assertThat(results.getRanges().get("baz").get("0 to 100 inc")).isEqualTo(1);
    }

    @Test
    public void healthCheckShouldSucceed() throws IOException {
        final Response healthCheckResponse =
                APP.client().target("http://localhost:" + APP.getAdminPort() + "/healthcheck")
                .request()
                .get();

        assertThat(healthCheckResponse)
                .extracting(Response::getStatus)
                .isEqualTo(Response.Status.OK.getStatusCode());
    }

}
