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

package org.apache.couchdb.nouveau.core.lucene4;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.After;
import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.DoubleRange;
import org.apache.couchdb.nouveau.api.SearchHit;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;
import org.apache.couchdb.nouveau.api.document.DoubleDocValuesField;
import org.apache.couchdb.nouveau.api.document.DoubleField;
import org.apache.couchdb.nouveau.api.document.Field;
import org.apache.couchdb.nouveau.api.document.SortedDocValuesField;
import org.apache.couchdb.nouveau.api.document.SortedSetDocValuesField;
import org.apache.couchdb.nouveau.api.document.StoredDoubleField;
import org.apache.couchdb.nouveau.api.document.StoredStringField;
import org.apache.couchdb.nouveau.api.document.StringField;
import org.apache.couchdb.nouveau.api.document.TextField;
import org.apache.couchdb.nouveau.core.Index;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.facet.params.FacetSearchParams;
import org.apache.lucene.facet.range.RangeAccumulator;
import org.apache.lucene.facet.range.RangeFacetRequest;
import org.apache.lucene.facet.search.CountFacetRequest;
import org.apache.lucene.facet.search.FacetRequest;
import org.apache.lucene.facet.search.FacetResult;
import org.apache.lucene.facet.search.FacetResultNode;
import org.apache.lucene.facet.search.FacetsCollector;
import org.apache.lucene.facet.sortedset.SortedSetDocValuesAccumulator;
import org.apache.lucene.facet.sortedset.SortedSetDocValuesReaderState;
import org.apache.lucene.facet.taxonomy.CategoryPath;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.search.FieldDoc;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.MultiCollector;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.SearcherManager;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TopFieldCollector;
import org.apache.lucene.util.BytesRef;

class Lucene4Index extends Index {

    private static final Sort DEFAULT_SORT = new Sort(SortField.FIELD_SCORE,
            new SortField("_id", SortField.Type.STRING));
    private static final Pattern SORT_FIELD_RE = Pattern.compile("^([-+])?([\\.\\w]+)(?:<(\\w+)>)?$");

    private final Analyzer analyzer;
    private final IndexWriter writer;
    private final SearcherManager searcherManager;
    private volatile boolean isOpen = true;

    Lucene4Index(final Analyzer analyzer, final IndexWriter writer, final long updateSeq,
            final SearcherManager searcherManager) {
        super(updateSeq);
        this.analyzer = analyzer;
        this.writer = writer;
        this.searcherManager = searcherManager;
    }

    @Override
    public int doNumDocs() throws IOException {
        return writer.numDocs();
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
        writer.setCommitData(Collections.singletonMap("update_seq", Long.toString(updateSeq)));
        final boolean result = writer.hasUncommittedChanges();
        writer.commit();
        return result;
    }

    @Override
    public void doClose() throws IOException {
        writer.close();
        synchronized (this) {
            isOpen = false;
        }
    }

    @Override
    public boolean isOpen() {
        return isOpen;
    }

    @Override
    public SearchResults doSearch(final SearchRequest request) throws IOException {
        final Query query;
        try {
            query = newQueryParser().parse(request);
        } catch (final ParseException e) {
            throw new WebApplicationException(e.getMessage(), e, Status.BAD_REQUEST);
        }

        // Construct Collectors.
        var hitCollector = hitCollector(request);

        searcherManager.maybeRefreshBlocking();
        final IndexSearcher searcher = searcherManager.acquire();
        try {
            FacetsCollector countsCollector = null;
            if (request.hasCounts()) {
                countsCollector = countsCollector(searcher.getIndexReader(), request.getCounts());
            }
            FacetsCollector rangesCollector = null;
            if (request.hasRanges()) {
                rangesCollector = rangesCollector(searcher.getIndexReader(), request.getRanges());
            }

            searcher.search(query, MultiCollector.wrap(hitCollector, countsCollector, rangesCollector));
            return toSearchResults(request, searcher, hitCollector, countsCollector, rangesCollector);
        } catch (IllegalStateException e) {
            throw new WebApplicationException(e.getMessage(), e, Status.BAD_REQUEST);
        } finally {
            searcherManager.release(searcher);
        }
    }

    private TopFieldCollector hitCollector(final SearchRequest searchRequest) throws IOException {
        final Sort sort = toSort(searchRequest);

        final After after = searchRequest.getAfter();
        final FieldDoc fieldDoc;
        if (after != null) {
            fieldDoc = toFieldDoc(after);
            if (getLastSortField(sort).getReverse()) {
                fieldDoc.doc = 0;
            } else {
                fieldDoc.doc = Integer.MAX_VALUE;
            }
        } else {
            fieldDoc = null;
        }

        return TopFieldCollector.create(
                sort,
                searchRequest.getLimit(),
                fieldDoc,
                true,
                false,
                false,
                false);
    }

    private FacetsCollector countsCollector(final IndexReader reader, List<String> counts) throws IOException {
        var state = new SortedSetDocValuesReaderState(reader);
        final FacetRequest[] facetRequests = new FacetRequest[counts.size()];
        for (int i = 0; i < facetRequests.length; i++) {
            facetRequests[i] = new CountFacetRequest(new CategoryPath(counts.get(i)), Integer.MAX_VALUE);
        }
        var facetSearchParams = new FacetSearchParams(facetRequests);
        var acc = new SortedSetDocValuesAccumulator(state, facetSearchParams);
        return FacetsCollector.create(acc);
    }

    private FacetsCollector rangesCollector(final IndexReader reader, Map<String, List<DoubleRange>> ranges)
            throws IOException {
        final List<FacetRequest> facetRequests = new ArrayList<FacetRequest>(ranges.size());
        for (final Entry<String, List<DoubleRange>> e : ranges.entrySet()) {
            final org.apache.lucene.facet.range.DoubleRange[] out = new org.apache.lucene.facet.range.DoubleRange[e
                    .getValue().size()];
            for (int i = 0; i < out.length; i++) {
                final DoubleRange in = e.getValue().get(i);
                out[i] = new org.apache.lucene.facet.range.DoubleRange(
                        in.getLabel(),
                        in.getMin(),
                        in.isMinInclusive(),
                        in.getMax(),
                        in.isMaxInclusive());
            }
            facetRequests.add(new RangeFacetRequest<org.apache.lucene.facet.range.DoubleRange>(
                    e.getKey(), out));
        }
        var acc = new RangeAccumulator(facetRequests);
        return FacetsCollector.create(acc);
    }

    private SortField getLastSortField(final Sort sort) {
        final SortField[] sortFields = sort.getSort();
        return sortFields[sortFields.length - 1];
    }

    private SearchResults toSearchResults(final SearchRequest searchRequest, final IndexSearcher searcher,
        TopFieldCollector hitCollector, FacetsCollector countsCollector, FacetsCollector rangesCollector) throws IOException {
        final SearchResults result = new SearchResults();
        collectHits(searcher, hitCollector.topDocs(), result);
        if (searchRequest.hasCounts()) {
            result.setCounts(convertFacets(countsCollector));
        }
        if (searchRequest.hasRanges()) {
            result.setRanges(convertFacets(rangesCollector));
        }
        return result;
    }

    private void collectHits(final IndexSearcher searcher, final TopDocs topDocs, final SearchResults searchResults)
            throws IOException {
        final List<SearchHit> hits = new ArrayList<SearchHit>(topDocs.scoreDocs.length);

        for (final ScoreDoc scoreDoc : topDocs.scoreDocs) {
            final Document doc = searcher.doc(scoreDoc.doc);

            final List<Field> fields = new ArrayList<Field>(doc.getFields().size());
            for (IndexableField field : doc.getFields()) {
                if (!field.name().equals("_id")) {
                    fields.add(toField(field));
                }
            }

            final After after = toAfter(((FieldDoc)scoreDoc));
            hits.add(new SearchHit(doc.get("_id"), after, fields));
        }

        searchResults.setTotalHits(topDocs.totalHits);
        searchResults.setHits(hits);
    }

    private Map<String,Map<String,Number>> convertFacets(final FacetsCollector fc) throws IOException {
        final Map<String,Map<String,Number>> result = new HashMap<String,Map<String,Number>>();
        for (final FacetResult facetResult : fc.getFacetResults()) {
            final FacetResultNode node = facetResult.getFacetResultNode();
            final Map<String, Number> m = result.computeIfAbsent(node.label.components[0], (k) -> new HashMap<String, Number>());
            m.put(node.label.components[1], node.value);
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

        for (Field field : request.getFields()) {
            // Underscore-prefix is reserved.
            if (field.getName().startsWith("_")) {
                continue;
            }
            result.add(toIndexableField(field));
        }

        return result;
    }

    private static IndexableField toIndexableField(final Field field) {
        if (field instanceof DoubleDocValuesField) {
            final DoubleDocValuesField f = (DoubleDocValuesField) field;
            return new org.apache.lucene.document.DoubleDocValuesField(f.getName(), f.getValue());
        }
        if (field instanceof DoubleField) {
            final DoubleField f = (DoubleField) field;
            return new org.apache.lucene.document.DoubleField(f.getName(), f.getValue(), f.isStored() ? Store.YES : Store.NO);
        }
        if (field instanceof SortedDocValuesField) {
            final SortedDocValuesField f = (SortedDocValuesField) field;
            return new org.apache.lucene.document.SortedDocValuesField(f.getName(), new BytesRef(f.getValue()));
        }
        if (field instanceof SortedSetDocValuesField) {
            final SortedSetDocValuesField f = (SortedSetDocValuesField) field;
            return new org.apache.lucene.document.SortedSetDocValuesField(f.getName(), new BytesRef(f.getValue()));
        }
        if (field instanceof StoredDoubleField) {
            final StoredDoubleField f = (StoredDoubleField) field;
            return new org.apache.lucene.document.StoredField(f.getName(), f.getValue());
        }
        if (field instanceof StoredStringField) {
            final StoredStringField f = (StoredStringField) field;
            return new org.apache.lucene.document.StoredField(f.getName(), f.getValue());
        }
        if (field instanceof StringField) {
            final StringField f = (StringField) field;
            return new org.apache.lucene.document.StringField(f.getName(), f.getValue(),
                f.isStored() ? Store.YES : Store.NO);
        }
        if (field instanceof TextField) {
            final StringField f = (StringField) field;
            return new org.apache.lucene.document.TextField(f.getName(), f.getValue(),
                f.isStored() ? Store.YES : Store.NO);
        }
        throw new WebApplicationException(field + " is not valid", Status.BAD_REQUEST);
    }

    private static Field toField(final IndexableField field) {
        if (field instanceof org.apache.lucene.document.DoubleDocValuesField) {
            final org.apache.lucene.document.DoubleDocValuesField f = (org.apache.lucene.document.DoubleDocValuesField) field;
            return new DoubleDocValuesField(f.name(), (double) f.numericValue());
        }
        if (field instanceof org.apache.lucene.document.DoubleField) {
            final org.apache.lucene.document.DoubleField f = (org.apache.lucene.document.DoubleField) field;
            return new DoubleField(f.name(), (double) f.numericValue(), f.fieldType().stored());
        }
        if (field instanceof org.apache.lucene.document.SortedDocValuesField) {
            final org.apache.lucene.document.SortedDocValuesField f = (org.apache.lucene.document.SortedDocValuesField) field;
            return new SortedDocValuesField(f.name(), toBytes(f.binaryValue()));
        }
        if (field instanceof org.apache.lucene.document.SortedSetDocValuesField) {
            final org.apache.lucene.document.SortedSetDocValuesField f = (org.apache.lucene.document.SortedSetDocValuesField) field;
            return new SortedSetDocValuesField(f.name(), toBytes(f.binaryValue()));
        }
        if (field instanceof org.apache.lucene.document.StoredField) {
            final org.apache.lucene.document.StoredField f = (org.apache.lucene.document.StoredField) field;
            if (f.stringValue() != null) {
                return new StoredStringField(f.name(), f.stringValue());
            }
            if (f.numericValue() != null && f.numericValue() instanceof Double) {
                return new StoredDoubleField(f.name(), (Double) f.numericValue());
            }
        }
        if (field instanceof org.apache.lucene.document.StringField) {
            final org.apache.lucene.document.StringField f = (org.apache.lucene.document.StringField) field;
            return new StringField(f.name(), f.stringValue(), f.fieldType().stored());
        }
        if (field instanceof org.apache.lucene.document.TextField) {
            final org.apache.lucene.document.StringField f = (org.apache.lucene.document.StringField) field;
            return new TextField(f.name(), f.stringValue(), f.fieldType().stored());
        }
        throw new WebApplicationException(field + " is not valid", Status.BAD_REQUEST);
    }

    private FieldDoc toFieldDoc(final After after) {
        final Object[] fields = Arrays.copyOf(after.getFields(), after.getFields().length);
        for (int i = 0; i < fields.length; i++) {
            if (fields[i] instanceof byte[]) {
                fields[i] = new BytesRef((byte[])fields[i]);
            }
        }
        return new FieldDoc(0, Float.NaN, fields);
    }

    private After toAfter(final FieldDoc fieldDoc) {
        final Object[] fields = Arrays.copyOf(fieldDoc.fields, fieldDoc.fields.length);
        for (int i = 0; i < fields.length; i++) {
            if (fields[i] instanceof BytesRef) {
                fields[i] = toBytes((BytesRef)fields[i]);
            }
        }
        return new After(fields);
    }

    private static byte[] toBytes(final BytesRef bytesRef) {
        return Arrays.copyOfRange(bytesRef.bytes, bytesRef.offset, bytesRef.offset + bytesRef.length);
    }

    private static Query docIdQuery(final String docId) {
        return new TermQuery(docIdTerm(docId));
    }

    private static Term docIdTerm(final String docId) {
        return new Term("_id", docId);
    }

    public Lucene4QueryParser newQueryParser() {
        return new Lucene4QueryParser("default", analyzer);
    }

    @Override
    public String toString() {
        return "Lucene4Index [analyzer=" + analyzer + ", writer=" + writer + ", searcherManager=" + searcherManager
                + ", isOpen=" + isOpen + "]";
    }

}
