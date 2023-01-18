package org.apache.couchdb.nouveau.api;

import static io.dropwizard.testing.FixtureHelpers.fixture;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Map;

import org.apache.couchdb.nouveau.core.ser.LuceneModule;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.apache.lucene.facet.range.DoubleRange;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class SearchRequestTest {

    private static ObjectMapper mapper;

    @BeforeAll
    public static void setupMapper() {
        mapper = new ObjectMapper();
        mapper.registerModule(new LuceneModule());
    }

    @Test
    public void testSerialisation() throws Exception {
        SearchRequest request = asObject();
        final String expected = mapper.writeValueAsString(
                mapper.readValue(fixture("fixtures/SearchRequest.json"), SearchRequest.class));
        assertThat(mapper.writeValueAsString(request)).isEqualTo(expected);
    }

    @Test
    public void testDeserialisation() throws Exception {
        SearchRequest request = asObject();
        assertThat(mapper.readValue(fixture("fixtures/SearchRequest.json"), SearchRequest.class).toString())
                .isEqualTo(request.toString());
    }

    private SearchRequest asObject() {
        final SearchRequest result = new SearchRequest();
        result.setQuery("*:*");
        result.setLimit(10);
        result.setCounts(List.of("bar"));
        result.setRanges(Map.of("foo", List.of(new DoubleRange("0 to 100 inc", 0.0, true, 100.0, true))));
        return result;
    }

}
