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

package org.apache.couchdb.nouveau.lucene;

import java.util.Map;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.queryparser.flexible.core.QueryNodeException;
import org.apache.lucene.queryparser.flexible.core.QueryParserHelper;
import org.apache.lucene.queryparser.flexible.standard.builders.StandardQueryTreeBuilder;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;
import org.apache.lucene.queryparser.flexible.standard.config.StandardQueryConfigHandler;
import org.apache.lucene.queryparser.flexible.standard.config.StandardQueryConfigHandler.ConfigurationKeys;
import org.apache.lucene.queryparser.flexible.standard.parser.StandardSyntaxParser;
import org.apache.lucene.queryparser.flexible.standard.processors.StandardQueryNodeProcessorPipeline;
import org.apache.lucene.search.Query;

public final class NouveauQueryParser extends QueryParserHelper {

    public NouveauQueryParser(final Analyzer analyzer, final Map<String, PointsConfig> pointsConfigMap) {
        super(
                new StandardQueryConfigHandler(),
                new StandardSyntaxParser(),
                new StandardQueryNodeProcessorPipeline(null),
                new StandardQueryTreeBuilder());
        getQueryConfigHandler().set(ConfigurationKeys.ENABLE_POSITION_INCREMENTS, true);
        getQueryConfigHandler().set(ConfigurationKeys.ANALYZER, analyzer);
        getQueryConfigHandler().set(ConfigurationKeys.POINTS_CONFIG_MAP, pointsConfigMap);
    }

    @Override
    public Query parse(String query, String defaultField) throws QueryNodeException {
        return (Query) super.parse(query, defaultField);
    }
}
