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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.XYPointField;
import org.apache.lucene.queryparser.flexible.core.QueryNodeException;
import org.apache.lucene.queryparser.flexible.core.QueryParserHelper;
import org.apache.lucene.queryparser.flexible.core.builders.QueryTreeBuilder;
import org.apache.lucene.queryparser.flexible.core.nodes.BooleanQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.BoostQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.FieldQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.FieldableNode;
import org.apache.lucene.queryparser.flexible.core.nodes.FuzzyQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.GroupQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.MatchAllDocsQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.MatchNoDocsQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.ModifierQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.QueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.QueryNodeImpl;
import org.apache.lucene.queryparser.flexible.core.nodes.RangeQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.SlopQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.TokenizedPhraseQueryNode;
import org.apache.lucene.queryparser.flexible.core.parser.EscapeQuerySyntax;
import org.apache.lucene.queryparser.flexible.core.processors.NoChildOptimizationQueryNodeProcessor;
import org.apache.lucene.queryparser.flexible.core.processors.QueryNodeProcessorImpl;
import org.apache.lucene.queryparser.flexible.core.processors.QueryNodeProcessorPipeline;
import org.apache.lucene.queryparser.flexible.core.processors.RemoveDeletedQueryNodesProcessor;
import org.apache.lucene.queryparser.flexible.standard.builders.BooleanQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.BoostQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.DummyQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.FieldQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.FuzzyQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.GroupQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.IntervalQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.MatchAllDocsQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.MatchNoDocsQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.MinShouldMatchNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.ModifierQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.MultiPhraseQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.PhraseQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.PointRangeQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.PrefixWildcardQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.RegexpQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.SlopQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.StandardQueryBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.SynonymQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.TermRangeQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.builders.WildcardQueryNodeBuilder;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;
import org.apache.lucene.queryparser.flexible.standard.config.StandardQueryConfigHandler;
import org.apache.lucene.queryparser.flexible.standard.config.StandardQueryConfigHandler.ConfigurationKeys;
import org.apache.lucene.queryparser.flexible.standard.nodes.IntervalQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.MinShouldMatchNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.MultiPhraseQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.PointQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.PointRangeQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.PrefixWildcardQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.RegexpQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.SynonymQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.TermRangeQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.WildcardQueryNode;
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

    public NouveauQueryParser(final Analyzer analyzer) {
        super(
                new StandardQueryConfigHandler(),
                new StandardSyntaxParser(),
                new NouveauQueryNodeProcessorPipeline(),
                new NouveauQueryTreeBuilder());
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
    private static class NouveauQueryNodeProcessorPipeline extends QueryNodeProcessorPipeline {

        public NouveauQueryNodeProcessorPipeline() {
            super(null);
            add(new WildcardQueryNodeProcessor());
            add(new MultiFieldQueryNodeProcessor());
            add(new FuzzyQueryNodeProcessor());
            add(new RegexpQueryNodeProcessor());
            add(new MatchAllDocsQueryNodeProcessor());
            add(new OpenRangeQueryNodeProcessor());
            add(new NouveauPointProcessor());
            add(new NouveauXYProcessor());
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

    private static class NouveauQueryTreeBuilder extends QueryTreeBuilder implements StandardQueryBuilder {

        public NouveauQueryTreeBuilder() {
            setBuilder(GroupQueryNode.class, new GroupQueryNodeBuilder());
            setBuilder(FieldQueryNode.class, new FieldQueryNodeBuilder());
            setBuilder(BooleanQueryNode.class, new BooleanQueryNodeBuilder());
            setBuilder(FuzzyQueryNode.class, new FuzzyQueryNodeBuilder());
            setBuilder(PointQueryNode.class, new DummyQueryNodeBuilder());
            setBuilder(PointRangeQueryNode.class, new PointRangeQueryNodeBuilder());
            setBuilder(BoostQueryNode.class, new BoostQueryNodeBuilder());
            setBuilder(ModifierQueryNode.class, new ModifierQueryNodeBuilder());
            setBuilder(WildcardQueryNode.class, new WildcardQueryNodeBuilder());
            setBuilder(TokenizedPhraseQueryNode.class, new PhraseQueryNodeBuilder());
            setBuilder(MatchNoDocsQueryNode.class, new MatchNoDocsQueryNodeBuilder());
            setBuilder(PrefixWildcardQueryNode.class, new PrefixWildcardQueryNodeBuilder());
            setBuilder(TermRangeQueryNode.class, new TermRangeQueryNodeBuilder());
            setBuilder(RegexpQueryNode.class, new RegexpQueryNodeBuilder());
            setBuilder(SlopQueryNode.class, new SlopQueryNodeBuilder());
            setBuilder(SynonymQueryNode.class, new SynonymQueryNodeBuilder());
            setBuilder(MultiPhraseQueryNode.class, new MultiPhraseQueryNodeBuilder());
            setBuilder(MatchAllDocsQueryNode.class, new MatchAllDocsQueryNodeBuilder());
            setBuilder(MinShouldMatchNode.class, new MinShouldMatchNodeBuilder());
            setBuilder(IntervalQueryNode.class, new IntervalQueryNodeBuilder());
            setBuilder(XYBoxQueryNode.class, new XYBoxQueryNodeBuilder());
        }

        @Override
        public Query build(QueryNode queryNode) throws QueryNodeException {
            return (Query) super.build(queryNode);
        }
    }

    /**
     * If it looks like a number, treat it as a number.
     */
    private static class NouveauPointProcessor extends QueryNodeProcessorImpl {

        @Override
        protected QueryNode postProcessNode(final QueryNode node) throws QueryNodeException {
            final var numberFormat = NumberFormat.getInstance();
            final var pointsConfig = new PointsConfig(numberFormat, Double.class);

            if (node instanceof FieldQueryNode && !(node.getParent() instanceof RangeQueryNode)) {
                final var fieldNode = (FieldQueryNode) node;
                String text = fieldNode.getTextAsString();
                if (text.length() == 0) {
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

                return new PointRangeQueryNode(
                        lowerNode, upperNode, lowerInclusive, upperInclusive, pointsConfig);
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

    private static class NouveauXYProcessor extends QueryNodeProcessorImpl {

        private final Pattern p = Pattern
                .compile("%(\\d+(?:\\.\\d+)?),(\\d+(?:\\.\\d+)?),(\\d+(?:\\.\\d+)?),(\\d+(?:\\.\\d+)?)");

        @Override
        protected QueryNode postProcessNode(final QueryNode node) throws QueryNodeException {
            if (node instanceof FieldQueryNode && !(node.getParent() instanceof RangeQueryNode)) {
                final var fieldNode = (FieldQueryNode) node;
                String text = fieldNode.getTextAsString();
                if (text.length() == 0) {
                    return node;
                }
                final Matcher m = p.matcher(text);
                if (m.matches()) {
                    System.err.println("YAY");
                    final float minX = Float.parseFloat(m.group(1));
                    final float maxX = Float.parseFloat(m.group(2));
                    final float minY = Float.parseFloat(m.group(3));
                    final float maxY = Float.parseFloat(m.group(4));
                    return new XYBoxQueryNode(fieldNode.getFieldAsString(), minX, maxX, minY, maxY);
                }
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

    private static class XYBoxQueryNode extends QueryNodeImpl implements FieldableNode {

        private CharSequence field;

        private final float minX, maxX, minY, maxY;

        public XYBoxQueryNode(String field, float minX, float maxX, float minY, float maxY) {
            this.field = field;
            this.minX = minX;
            this.maxX = maxX;
            this.minY = minY;
            this.maxY = maxY;
        }

        @Override
        public CharSequence toQueryString(EscapeQuerySyntax escapeSyntaxParser) {
            final String value = String.format("<%f,%f,%f,%f>", minX, maxX, minY, maxY);
            if (isDefaultField(this.field)) {
                return value;
            } else {
                return this.field + ":" + value;
            }
        }

        @Override
        public CharSequence getField() {
            return field;
        }

        public float getMinX() {
            return minX;
        }

        public float getMinY() {
            return minY;
        }

        public float getMaxX() {
            return maxX;
        }

        public float getMaxY() {
            return maxY;
        }

        @Override
        public void setField(CharSequence field) {
            this.field = field;
        }
    }

    private static class XYBoxQueryNodeBuilder implements StandardQueryBuilder {

        @Override
        public Query build(QueryNode queryNode) throws QueryNodeException {
            var n = (XYBoxQueryNode) queryNode;
            return XYPointField.newBoxQuery(n.getField().toString(), n.getMinX(), n.getMaxX(), n.getMinY(),
                    n.getMaxY());
        }
    }

}