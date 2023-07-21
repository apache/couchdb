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

package org.apache.couchdb.nouveau.lucene9;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.List;
import java.util.Locale;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.queryparser.flexible.core.QueryNodeException;
import org.apache.lucene.queryparser.flexible.core.QueryParserHelper;
import org.apache.lucene.queryparser.flexible.core.nodes.FieldQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.QueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.RangeQueryNode;
import org.apache.lucene.queryparser.flexible.core.processors.NoChildOptimizationQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.core.processors.QueryNodeProcessorImpl;
import org.apache.lucene.queryparser.flexible.core.processors.QueryNodeProcessorPipeline;
import org.apache.lucene.queryparser.flexible.core.processors.RemoveDeletedQueryNodesProcessor;
import org.apache.lucene.queryparser.flexible.standard.builders.StandardQueryTreeBuilder;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;
import org.apache.lucene.queryparser.flexible.standard.config.StandardQueryConfigHandler;
import org.apache.lucene.queryparser.flexible.standard.config.StandardQueryConfigHandler.ConfigurationKeys;
import org.apache.lucene.queryparser.flexible.standard.nodes.PointQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.PointRangeQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.TermRangeQueryNode;
import org.apache.lucene.queryparser.flexible.standard.parser.StandardSyntaxParser;
import org.apache.lucene.queryparser.flexible.standard.processors.AllowLeadingWildcardProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.AnalyzerQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.BooleanQuery2ModifierNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.BooleanSingleChildOptimizationQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.BoostQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.DefaultPhraseSlopQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.FuzzyQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.IntervalQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.MatchAllDocsQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.MultiFieldQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.MultiTermRewriteMethodProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.OpenRangeQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.PhraseSlopQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.RegexpQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.RemoveEmptyNonLeafQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.TermRangeQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.standard.processors.WildcardQueryNodeProcessor;
import org.apache.lucene.search.Query;

public final class NouveauQueryParser extends QueryParserHelper {

    public NouveauQueryParser(final Analyzer analyzer, final Locale locale) {
        super(
                new StandardQueryConfigHandler(),
                new StandardSyntaxParser(),
                new NouveauQueryNodeProcessorPipeline(locale),
                new StandardQueryTreeBuilder());
        getQueryConfigHandler().set(ConfigurationKeys.ENABLE_POSITION_INCREMENTS, true);
        getQueryConfigHandler().set(ConfigurationKeys.ANALYZER, analyzer);
    }

    @Override
    public Query parse(String query, String defaultField) throws QueryNodeException {
        return (Query) super.parse(query, defaultField);
    }

    /**
     * Same pipeline as StandardQueryParser but we substitute
     * PointQueryNodeProcessor and PointRangeQueryNodeProcessor for
     * NouveauPointProcessor below.
     */
    public static class NouveauQueryNodeProcessorPipeline extends QueryNodeProcessorPipeline {

        public NouveauQueryNodeProcessorPipeline(final Locale locale) {
            super(null);
            add(new WildcardQueryNodeProcessor());
            add(new MultiFieldQueryNodeProcessor());
            add(new FuzzyQueryNodeProcessor());
            add(new RegexpQueryNodeProcessor());
            add(new MatchAllDocsQueryNodeProcessor());
            add(new OpenRangeQueryNodeProcessor());
            add(new NouveauPointProcessor(locale));
            add(new TermRangeQueryNodeProcessor());
            add(new AllowLeadingWildcardProcessor());
            add(new AnalyzerQueryNodeProcessor());
            add(new PhraseSlopQueryNodeProcessor());
            add(new BooleanQuery2ModifierNodeProcessor());
            add(new NoChildOptimizationQueryNodeProcessor());
            add(new RemoveDeletedQueryNodesProcessor());
            add(new RemoveEmptyNonLeafQueryNodeProcessor());
            add(new BooleanSingleChildOptimizationQueryNodeProcessor());
            add(new DefaultPhraseSlopQueryNodeProcessor());
            add(new BoostQueryNodeProcessor());
            add(new MultiTermRewriteMethodProcessor());
            add(new IntervalQueryNodeProcessor());
        }
    }

    /**
     * If it looks like a number, treat it as a number.
     */
    public static class NouveauPointProcessor extends QueryNodeProcessorImpl {

        private final Locale locale;

        NouveauPointProcessor(final Locale locale) {
            this.locale = locale != null ? locale : Locale.getDefault();
        }

        @Override
        protected QueryNode postProcessNode(final QueryNode node) throws QueryNodeException {
            final var numberFormat = NumberFormat.getInstance(locale);
            final var pointsConfig = new PointsConfig(numberFormat, Double.class);

            if (node instanceof FieldQueryNode && !(node.getParent() instanceof RangeQueryNode)) {
                final var fieldNode = (FieldQueryNode) node;
                String text = fieldNode.getTextAsString();
                if (text.isEmpty()) {
                    return node;
                }
                final Number number;
                try {
                    number = numberFormat.parse(text).doubleValue();
                } catch (final ParseException e) {
                    return node;
                }
                final var lowerNode = new PointQueryNode(fieldNode.getField(), number, numberFormat);
                final var upperNode = new PointQueryNode(fieldNode.getField(), number, numberFormat);
                return new PointRangeQueryNode(lowerNode, upperNode, true, true, pointsConfig);
            }

            if (node instanceof TermRangeQueryNode) {
                final var termRangeNode = (TermRangeQueryNode) node;
                final var lower = termRangeNode.getLowerBound();
                final var upper = termRangeNode.getUpperBound();
                final var lowerText = lower.getTextAsString();
                final var upperText = upper.getTextAsString();
                Number lowerNumber = null, upperNumber = null;

                if (lowerText.length() > 0 && !lowerText.equals("-Infinity")) {
                    try {
                        lowerNumber = numberFormat.parse(lowerText).doubleValue();
                    } catch (final ParseException e) {
                        return node;
                    }
                }

                if (upperText.length() > 0 && !upperText.equals("Infinity")) {
                    try {
                        upperNumber = numberFormat.parse(upperText).doubleValue();
                    } catch (final ParseException e) {
                        return node;
                    }
                }

                final var lowerNode = new PointQueryNode(termRangeNode.getField(), lowerNumber, numberFormat);
                final var upperNode = new PointQueryNode(termRangeNode.getField(), upperNumber, numberFormat);
                final var lowerInclusive = termRangeNode.isLowerInclusive();
                final var upperInclusive = termRangeNode.isUpperInclusive();

                return new PointRangeQueryNode(lowerNode, upperNode, lowerInclusive, upperInclusive, pointsConfig);
            }

            return node;
        }

        @Override
        protected QueryNode preProcessNode(final QueryNode node) throws QueryNodeException {
            return node;
        }

        @Override
        protected List<QueryNode> setChildrenOrder(final List<QueryNode> children) throws QueryNodeException {
            return children;
        }
    }
}
