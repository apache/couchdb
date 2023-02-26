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

import java.util.regex.Pattern;

import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.NumericRangeQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.BooleanClause.Occur;

import org.apache.lucene.analysis.core.KeywordAnalyzer;

class Lucene4QueryParser extends QueryParser {

    private static final Pattern DOUBLE_REGEX;

    static {
        final String Digits = "(\\p{Digit}+)";
        final String HexDigits = "(\\p{XDigit}+)";
        // an exponent is 'e' or 'E' followed by an optionally
        // signed decimal integer.
        final String Exp = "[eE][+-]?" + Digits;
        final String fpRegex = ("[\\x00-\\x20]*" + // Optional leading "whitespace"
                "[+-]?(" + // Optional sign character
                "NaN|" + // "NaN" string
                "Infinity|" + // "Infinity" string

                // A decimal floating-point string representing a finite positive
                // number without a leading sign has at most five basic pieces:
                // Digits . Digits ExponentPart FloatTypeSuffix
                //
                // Since this method allows integer-only strings as input
                // in addition to strings of floating-point literals, the
                // two sub-patterns below are simplifications of the grammar
                // productions from section 3.10.2 of
                // The Java Language Specification.

                // Digits ._opt Digits_opt ExponentPart_opt FloatTypeSuffix_opt
                "(((" + Digits + "(\\.)?(" + Digits + "?)(" + Exp + ")?)|" +

                // . Digits ExponentPart_opt FloatTypeSuffix_opt
                "(\\.(" + Digits + ")(" + Exp + ")?)|" +

                // Hexadecimal strings
                "((" +
                // 0[xX] HexDigits ._opt BinaryExponent FloatTypeSuffix_opt
                "(0[xX]" + HexDigits + "(\\.)?)|" +

                // 0[xX] HexDigits_opt . HexDigits BinaryExponent FloatTypeSuffix_opt
                "(0[xX]" + HexDigits + "?(\\.)" + HexDigits + ")" +

                ")[pP][+-]?" + Digits + "))" +
                "[fFdD]?))" +
                "[\\x00-\\x20]*");// Optional trailing "whitespace"

        DOUBLE_REGEX = Pattern.compile(fpRegex);
    }

    Lucene4QueryParser(final String defaultField, final Analyzer analyzer) {
        super(Utils.LUCENE_VERSION, defaultField, analyzer);
    }

    @Override
    protected org.apache.lucene.search.Query getFieldQuery(String field, String queryText, boolean quoted)
            throws ParseException {
        if (!quoted && isNumber(queryText)) {
            return new TermQuery(Utils.doubleToTerm(field, Double.parseDouble(queryText)));
        }
        return super.getFieldQuery(field, queryText, quoted);
    }

    @Override
    protected org.apache.lucene.search.Query getFuzzyQuery(String field, String termStr, float minSimilarity)
            throws ParseException {
        setLowercaseExpandedTerms(field);
        return super.getFuzzyQuery(field, termStr, minSimilarity);
    }

    @Override
    protected org.apache.lucene.search.Query getPrefixQuery(String field, String termStr) throws ParseException {
        setLowercaseExpandedTerms(field);
        return super.getPrefixQuery(field, termStr);
    }

    @Override
    protected org.apache.lucene.search.Query getRangeQuery(String field, String lower, String upper,
            boolean startInclusive, boolean endInclusive) throws ParseException {
        if (isNumber(lower) && isNumber(upper)) {
            return NumericRangeQuery.newDoubleRange(field, 8, Double.parseDouble(lower),
                    Double.parseDouble(upper), startInclusive, endInclusive);
        }
        setLowercaseExpandedTerms(field);
        return super.getRangeQuery(field, lower, upper, startInclusive, endInclusive);

    }

    @Override
    protected org.apache.lucene.search.Query getRegexpQuery(String field, String termStr) throws ParseException {
        setLowercaseExpandedTerms(field);
        return super.getRegexpQuery(field, termStr);
    }

    @Override
    protected org.apache.lucene.search.Query getWildcardQuery(String field, String termStr) throws ParseException {
        setLowercaseExpandedTerms(field);
        return super.getWildcardQuery(field, termStr);
    }

    private static boolean isNumber(String str) {
        return DOUBLE_REGEX.matcher(str).matches();
    }

    private void setLowercaseExpandedTerms(String field) {
        Analyzer analyzer = getAnalyzer();
        if (analyzer instanceof PerFieldAnalyzer) {
            setLowercaseExpandedTerms(((PerFieldAnalyzer) analyzer).getWrappedAnalyzer(field));
        } else {
            setLowercaseExpandedTerms(analyzer);
        }
    }

    private void setLowercaseExpandedTerms(Analyzer analyzer) {
        setLowercaseExpandedTerms(!(analyzer instanceof KeywordAnalyzer));
    }

    public Query parse(SearchRequest searchRequest) throws ParseException {
        final Query q = (Query) parse(searchRequest.getQuery());
        if (searchRequest.hasPartition()) {
            final BooleanQuery result = new BooleanQuery();
            result.add(new TermQuery(new org.apache.lucene.index.Term("_partition",
                    searchRequest.getPartition())), Occur.MUST);
            result.add(q, Occur.MUST);
            return result;
        }
        return q;
    }

}
