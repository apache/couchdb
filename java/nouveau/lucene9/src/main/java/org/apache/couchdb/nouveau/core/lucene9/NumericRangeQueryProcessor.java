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

package org.apache.couchdb.nouveau.core.lucene9;

import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.List;

import org.apache.lucene.queryparser.flexible.core.QueryNodeException;
import org.apache.lucene.queryparser.flexible.core.nodes.FieldQueryNode;
import org.apache.lucene.queryparser.flexible.core.nodes.QueryNode;
import org.apache.lucene.queryparser.flexible.core.processors.QueryNodeProcessorImpl;
import org.apache.lucene.queryparser.flexible.standard.config.PointsConfig;
import org.apache.lucene.queryparser.flexible.standard.nodes.PointQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.PointRangeQueryNode;
import org.apache.lucene.queryparser.flexible.standard.nodes.TermRangeQueryNode;

class NumericRangeQueryProcessor extends QueryNodeProcessorImpl {

    // TODO don't like this is locale dependent.
    private final NumberFormat decimalFormat = NumberFormat.getInstance();
    private final PointsConfig doublePointsConfig = new PointsConfig(decimalFormat, Double.class);

    @Override
    protected QueryNode preProcessNode(QueryNode node) throws QueryNodeException {
        return node;
    }

    @Override
    protected QueryNode postProcessNode(QueryNode node) throws QueryNodeException {
        if (node instanceof TermRangeQueryNode) {
            final TermRangeQueryNode rangeNode = (TermRangeQueryNode) node;
            final Number lowerValue = toNumber(rangeNode.getLowerBound());
            final Number upperValue = toNumber(rangeNode.getUpperBound());
            if (lowerValue != null && upperValue != null) {
                return new PointRangeQueryNode(
                        toPointQueryNode(rangeNode.getField(), lowerValue),
                        toPointQueryNode(rangeNode.getField(), upperValue),
                        rangeNode.isLowerInclusive(),
                        rangeNode.isUpperInclusive(),
                        doublePointsConfig);
            }
        }
        return node;
    }

    @Override
    protected List<QueryNode> setChildrenOrder(List<QueryNode> children) throws QueryNodeException {
        return children;
    }

    private Number toNumber(final FieldQueryNode node) {
        switch (node.getTextAsString()) {
            case "Infinity":
                return Double.POSITIVE_INFINITY;
            case "-Infinity":
                return Double.NEGATIVE_INFINITY;
            default:
                return decimalFormat.parse(node.getTextAsString(), new ParsePosition(0)).doubleValue();
        }
    }

    private PointQueryNode toPointQueryNode(final CharSequence field, final Number value) {
        return new PointQueryNode(field, value, decimalFormat);
    }

}
