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

import java.io.Closeable;
import java.io.IOException;

import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexInfo;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;

public abstract class Index implements Closeable {

    private long updateSeq;

    private boolean deleteOnClose = false;

    protected Index(final long updateSeq) {
        this.updateSeq = updateSeq;
    }

    public final IndexInfo info() throws IOException {
        final int numDocs = doNumDocs();
        return new IndexInfo(updateSeq, numDocs);
    }

    protected abstract int doNumDocs() throws IOException;

    public final synchronized void update(final String docId, final DocumentUpdateRequest request) throws IOException {
        assertUpdateSeqIsLower(request.getSeq());
        doUpdate(docId, request);
        incrementUpdateSeq(request.getSeq());
    }

    protected abstract void doUpdate(final String docId, final DocumentUpdateRequest request) throws IOException;

    public final synchronized void delete(final String docId, final DocumentDeleteRequest request) throws IOException {
        assertUpdateSeqIsLower(request.getSeq());
        doDelete(docId, request);
        incrementUpdateSeq(request.getSeq());
    }

    protected abstract void doDelete(final String docId, final DocumentDeleteRequest request) throws IOException;

    public final SearchResults search(final SearchRequest request) throws IOException {
        return doSearch(request);
    }

    protected abstract SearchResults doSearch(final SearchRequest request) throws IOException;

    public final boolean commit() throws IOException {
        final long updateSeq;
        synchronized (this) {
            updateSeq = this.updateSeq;
        }
        return doCommit(updateSeq);
    }

    protected abstract boolean doCommit(final long updateSeq) throws IOException;

    public final void close() throws IOException {
        doClose();
    }

    protected abstract void doClose() throws IOException;

    public abstract boolean isOpen();

    public boolean isDeleteOnClose() {
        return deleteOnClose;
    }

    public void setDeleteOnClose(final boolean deleteOnClose) {
        this.deleteOnClose = deleteOnClose;
    }

    protected final void assertUpdateSeqIsLower(final long updateSeq) throws UpdatesOutOfOrderException {
        if (!(updateSeq > this.updateSeq)) {
            throw new UpdatesOutOfOrderException();
        }
    }

    protected final void incrementUpdateSeq(final long updateSeq) throws IOException {
        assertUpdateSeqIsLower(updateSeq);
        this.updateSeq = updateSeq;
    }

}