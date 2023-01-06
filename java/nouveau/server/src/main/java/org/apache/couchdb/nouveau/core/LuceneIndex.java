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

package org.apache.couchdb.nouveau.core;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.SearchHit;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.facet.FacetResult;
import org.apache.lucene.facet.Facets;
import org.apache.lucene.facet.FacetsCollector;
import org.apache.lucene.facet.FacetsCollectorManager;
import org.apache.lucene.facet.LabelAndValue;
import org.apache.lucene.facet.StringDocValuesReaderState;
import org.apache.lucene.facet.StringValueFacetCounts;
import org.apache.lucene.facet.range.DoubleRange;
import org.apache.lucene.facet.range.DoubleRangeFacetCounts;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.index.Term;
import org.apache.lucene.misc.store.DirectIODirectory;
import org.apache.lucene.search.CollectorManager;
import org.apache.lucene.search.FieldDoc;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.MultiCollectorManager;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.SearcherFactory;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TopFieldCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;

class LuceneIndex extends Index {

    private static final DoubleRange[] EMPTY_DOUBLE_RANGE_ARRAY = new DoubleRange[0];
    private static final Sort DEFAULT_SORT = new Sort(SortField.FIELD_SCORE,
            new SortField("_id", SortField.Type.STRING));
    private static final Pattern SORT_FIELD_RE = Pattern.compile("^([-+])?([\\.\\w]+)(?:<(\\w+)>)?$");

    private final Analyzer analyzer;
    private final IndexWriter writer;
    private final SearcherManager searcherManager;

    static Index open(final Path path, final Analyzer analyzer, final SearcherFactory searcherFactory)
            throws IOException {
        final Directory dir = new DirectIODirectory(FSDirectory.open(path));
        final IndexWriterConfig config = new IndexWriterConfig(analyzer);
        config.setUseCompoundFile(false);
        final IndexWriter writer = new IndexWriter(dir, config);
        final long updateSeq = getUpdateSeq(writer);
        final SearcherManager searcherManager = new SearcherManager(writer, searcherFactory);
        return new LuceneIndex(analyzer, writer, updateSeq, searcherManager);
    }

    private LuceneIndex(final Analyzer analyzer, final IndexWriter writer, final long updateSeq,
            final SearcherManager searcherManager) {
        super(updateSeq);
        this.analyzer = analyzer;
        this.writer = writer;
        this.searcherManager = searcherManager;
    }

    @Override
    public int doNumDocs() throws IOException {
        return writer.getDocStats().numDocs;
    }

    @Override
    public void doUpdate(final String docId, final DocumentUpdateRequest request) throws IOException {
        final Term docIdTerm = docIdTerm(docId);
        final Document doc = toDocument(docId, request);
        writer.updateDocument(docIdTerm, doc);
    }

    @Override
    public void doDelete(final String docId, final DocumentDeleteRequest request) throws IOException {
        final Query query = docIdQuery(docId);
        writer.deleteDocuments(query);
    }

    @Override
    public boolean doCommit(final long updateSeq) throws IOException {
        writer.setLiveCommitData(Collections.singletonMap("update_seq", Long.toString(updateSeq)).entrySet());
        return writer.commit() != -1;
    }

    @Override
    public void doClose() throws IOException {
        writer.close();
    }

    @Override
    public boolean isOpen() {
        return writer.isOpen();
    }

    @Override
    public SearchResults doSearch(final SearchRequest request) throws IOException, QueryParserException {
        final Query query = newQueryParser().parse(request);

        // Construct CollectorManagers.
        final MultiCollectorManager cm;
        final CollectorManager<?, ? extends TopDocs> hits = hitCollector(request);

        searcherManager.maybeRefreshBlocking();

        final IndexSearcher searcher = searcherManager.acquire();
        try {
            if (request.hasCounts() || request.hasRanges()) {
                cm = new MultiCollectorManager(hits, new FacetsCollectorManager());
            } else {
                cm = new MultiCollectorManager(hits);
            }
            final Object[] reduces = searcher.search(query, cm);
            return toSearchResults(request, searcher, reduces);
        } catch (IllegalStateException e) {
            throw new WebApplicationException(e.getMessage(), e, Status.BAD_REQUEST);
        } finally {
            searcherManager.release(searcher);
        }
    }

    private CollectorManager<?, ? extends TopDocs> hitCollector(final SearchRequest searchRequest) {
        final Sort sort = toSort(searchRequest);

        final FieldDoc after = searchRequest.getAfter();
        if (after != null) {
            if (getLastSortField(sort).getReverse()) {
                after.doc = 0;
            } else {
                after.doc = Integer.MAX_VALUE;
            }
        }

        return TopFieldCollector.createSharedManager(
                sort,
                searchRequest.getLimit(),
                after,
                1000);
    }

    private SortField getLastSortField(final Sort sort) {
        final SortField[] sortFields = sort.getSort();
        return sortFields[sortFields.length - 1];
    }

    private SearchResults toSearchResults(final SearchRequest searchRequest, final IndexSearcher searcher,
            final Object[] reduces) throws IOException {
        final SearchResults result = new SearchResults();
        collectHits(searcher, (TopDocs) reduces[0], result);
        if (reduces.length == 2) {
            collectFacets(searchRequest, searcher, (FacetsCollector) reduces[1], result);
        }
        return result;
    }

    private void collectHits(final IndexSearcher searcher, final TopDocs topDocs, final SearchResults searchResults)
            throws IOException {
        final List<SearchHit> hits = new ArrayList<SearchHit>(topDocs.scoreDocs.length);

        for (final ScoreDoc scoreDoc : topDocs.scoreDocs) {
            final Document doc = searcher.doc(scoreDoc.doc);

            final List<IndexableField> fields = new ArrayList<IndexableField>(doc.getFields());
            for (IndexableField field : doc.getFields()) {
                if (field.name().equals("_id")) {
                    fields.remove(field);
                }
            }

            hits.add(new SearchHit(doc.get("_id"), (FieldDoc) scoreDoc, fields));
        }

        searchResults.setTotalHits(topDocs.totalHits.value);
        searchResults.setTotalHitsRelation(topDocs.totalHits.relation.name());
        searchResults.setHits(hits);
    }

    private void collectFacets(final SearchRequest searchRequest, final IndexSearcher searcher,
            final FacetsCollector fc, final SearchResults searchResults) throws IOException {
        if (searchRequest.hasCounts()) {
            final Map<String, Map<String, Number>> countsMap = new HashMap<String, Map<String, Number>>(
                    searchRequest.getCounts().size());
            for (final String field : searchRequest.getCounts()) {
                final StringDocValuesReaderState state = new StringDocValuesReaderState(searcher.getIndexReader(),
                        field);
                final StringValueFacetCounts counts = new StringValueFacetCounts(state, fc);
                countsMap.put(field, collectFacets(counts, searchRequest.getTopN(), field));
            }
            searchResults.setCounts(countsMap);
        }

        if (searchRequest.hasRanges()) {
            final Map<String, Map<String, Number>> rangesMap = new HashMap<String, Map<String, Number>>(
                    searchRequest.getRanges().size());
            for (final Entry<String, List<DoubleRange>> entry : searchRequest.getRanges().entrySet()) {
                final DoubleRangeFacetCounts counts = new DoubleRangeFacetCounts(entry.getKey(), fc,
                        entry.getValue().toArray(EMPTY_DOUBLE_RANGE_ARRAY));
                rangesMap.put(entry.getKey(), collectFacets(counts, searchRequest.getTopN(), entry.getKey()));
            }
            searchResults.setRanges(rangesMap);
        }
    }

    private Map<String, Number> collectFacets(final Facets facets, final int topN, final String dim)
            throws IOException {
        final FacetResult topChildren = facets.getTopChildren(topN, dim);
        final Map<String, Number> result = new HashMap<String, Number>(topChildren.childCount);
        for (final LabelAndValue lv : topChildren.labelValues) {
            result.put(lv.label, lv.value);
        }
        return result;
    }

    // Ensure _id is final sort field so we can paginate.
    private Sort toSort(final SearchRequest searchRequest) {
        if (!searchRequest.hasSort()) {
            return DEFAULT_SORT;
        }

        final List<String> sort = new ArrayList<String>(searchRequest.getSort());
        final String last = sort.get(sort.size() - 1);
        // Append _id field if not already present.
        switch (last) {
            case "-_id<string>":
            case "_id<string>":
                break;
            default:
                sort.add("_id<string>");
        }
        return convertSort(sort);
    }

    private Sort convertSort(final List<String> sort) {
        final SortField[] fields = new SortField[sort.size()];
        for (int i = 0; i < sort.size(); i++) {
            fields[i] = convertSortField(sort.get(i));
        }
        return new Sort(fields);
    }

    private SortField convertSortField(final String sortString) {
        final Matcher m = SORT_FIELD_RE.matcher(sortString);
        if (!m.matches()) {
            throw new WebApplicationException(
                    sortString + " is not a valid sort parameter", Status.BAD_REQUEST);
        }
        final boolean reverse = "-".equals(m.group(1));
        SortField.Type type = SortField.Type.DOUBLE;
        if ("string".equals(m.group(3))) {
            type = SortField.Type.STRING;
        }
        return new SortField(m.group(2), type, reverse);
    }

    private static Document toDocument(final String docId, final DocumentUpdateRequest request) throws IOException {
        final Document result = new Document();

        // id
        result.add(new org.apache.lucene.document.StringField("_id", docId, Store.YES));
        result.add(new org.apache.lucene.document.SortedDocValuesField("_id", new BytesRef(docId)));

        // partition (optional)
        if (request.hasPartition()) {
            result.add(new org.apache.lucene.document.StringField("_partition", request.getPartition(), Store.NO));
        }

        for (IndexableField field : request.getFields()) {
            // Underscore-prefix is reserved.
            if (field.name().startsWith("_")) {
                continue;
            }
            result.add(field);
        }

        return result;
    }

    private static Query docIdQuery(final String docId) {
        return new TermQuery(docIdTerm(docId));
    }

    private static Term docIdTerm(final String docId) {
        return new Term("_id", docId);
    }

    private static long getUpdateSeq(final IndexWriter writer) throws IOException {
        final Iterable<Map.Entry<String, String>> commitData = writer.getLiveCommitData();
        if (commitData == null) {
            return 0L;
        }
        for (Map.Entry<String, String> entry : commitData) {
            if (entry.getKey().equals("update_seq")) {
                return Long.parseLong(entry.getValue());
            }
        }
        return 0L;
    }

    public QueryParser newQueryParser() {
        return new NouveauQueryParser("default", analyzer);
    }

}
