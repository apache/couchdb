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

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response.Status;
import java.io.Closeable;
import java.io.IOException;
import org.apache.couchdb.nouveau.api.DocumentDeleteRequest;
import org.apache.couchdb.nouveau.api.DocumentUpdateRequest;
import org.apache.couchdb.nouveau.api.IndexInfo;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;

/**
 * An index that reflects a single `.couch` file shard of some
 * database.
 *
 * The class only permits sequential modification (updates and deletes)
 * but allows concurrent searching.
 *
 * This class also expects a monotonically incrementing update sequence
 * associated with each modification.
 */
public abstract class Index implements Closeable {

    private long updateSeq;
    private long committedUpdateSeq;
    private long purgeSeq;
    private boolean deleteOnClose = false;

    protected Index(final long updateSeq, final long purgeSeq) {
        this.updateSeq = updateSeq;
        this.committedUpdateSeq = updateSeq;
        this.purgeSeq = purgeSeq;
    }

    public final IndexInfo info() throws IOException {
        final int numDocs = doNumDocs();
        final long diskSize = doDiskSize();
        return new IndexInfo(updateSeq, committedUpdateSeq, purgeSeq, numDocs, diskSize);
    }

    protected abstract int doNumDocs() throws IOException;

    protected abstract long doDiskSize() throws IOException;

    public final synchronized void update(final String docId, final DocumentUpdateRequest request) throws IOException {
        assertUpdateSeqProgress(request.getMatchSeq(), request.getSeq());
        doUpdate(docId, request);
        incrementUpdateSeq(request.getMatchSeq(), request.getSeq());
    }

    protected abstract void doUpdate(final String docId, final DocumentUpdateRequest request) throws IOException;

    public final synchronized void delete(final String docId, final DocumentDeleteRequest request) throws IOException {
        if (request.isPurge()) {
            assertPurgeSeqProgress(request.getMatchSeq(), request.getSeq());
            doDelete(docId, request);
            incrementPurgeSeq(request.getMatchSeq(), request.getSeq());
        } else {
            assertUpdateSeqProgress(request.getMatchSeq(), request.getSeq());
            doDelete(docId, request);
            incrementUpdateSeq(request.getMatchSeq(), request.getSeq());
        }
    }

    protected abstract void doDelete(final String docId, final DocumentDeleteRequest request) throws IOException;

    public final SearchResults search(final SearchRequest request) throws IOException {
        assertMinSeqs(request.getMinUpdateSeq(), request.getMinPurgeSeq());
        return doSearch(request);
    }

    protected abstract SearchResults doSearch(final SearchRequest request) throws IOException;

    public final boolean commit() throws IOException {
        final long updateSeq;
        final long purgeSeq;
        synchronized (this) {
            if (deleteOnClose) {
                return false;
            }
            updateSeq = this.updateSeq;
            purgeSeq = this.purgeSeq;
        }
        final boolean result = doCommit(updateSeq, purgeSeq);
        if (result) {
            synchronized (this) {
                this.committedUpdateSeq = Math.max(this.committedUpdateSeq, updateSeq);
            }
        }
        return result;
    }

    protected abstract boolean doCommit(final long updateSeq, final long purgeSeq) throws IOException;

    public final synchronized void setUpdateSeq(final long matchSeq, final long updateSeq) throws IOException {
        if (updateSeq < this.updateSeq) {
            throw new WebApplicationException(
                    "update_seq must be equal or greater than current update_seq", Status.BAD_REQUEST);
        }
        if (updateSeq > this.updateSeq) {
            incrementUpdateSeq(matchSeq, updateSeq);
        }
    }

    public final synchronized void setPurgeSeq(final long matchSeq, final long purgeSeq) throws IOException {
        if (purgeSeq < this.purgeSeq) {
            throw new WebApplicationException(
                    "purge_seq must be equal or greater than current purge_seq", Status.BAD_REQUEST);
        }
        if (purgeSeq > this.purgeSeq) {
            incrementPurgeSeq(matchSeq, purgeSeq);
        }
    }

    @Override
    public final void close() throws IOException {
        doClose();
    }

    protected abstract void doClose() throws IOException;

    public synchronized boolean isDeleteOnClose() {
        return deleteOnClose;
    }

    public final boolean isOpen() {
        return doIsOpen();
    }

    protected abstract boolean doIsOpen();

    public synchronized void setDeleteOnClose(final boolean deleteOnClose) {
        this.deleteOnClose = deleteOnClose;
    }

    protected final void assertUpdateSeqProgress(final long matchSeq, final long updateSeq)
            throws UpdatesOutOfOrderException {
        assert Thread.holdsLock(this);
        if (matchSeq != this.updateSeq) {
            throw new UpdatesOutOfOrderException(false, this.updateSeq, matchSeq, updateSeq);
        }
        if (!(updateSeq > this.updateSeq)) {
            throw new UpdatesOutOfOrderException(false, this.updateSeq, matchSeq, updateSeq);
        }
    }

    protected final void incrementUpdateSeq(final long matchSeq, final long updateSeq) throws IOException {
        assert Thread.holdsLock(this);
        assertUpdateSeqProgress(matchSeq, updateSeq);
        this.updateSeq = updateSeq;
    }

    protected final void assertPurgeSeqProgress(final long matchSeq, final long purgeSeq)
            throws UpdatesOutOfOrderException {
        assert Thread.holdsLock(this);
        if (matchSeq != this.purgeSeq) {
            throw new UpdatesOutOfOrderException(true, this.purgeSeq, matchSeq, purgeSeq);
        }
        if (!(purgeSeq > this.purgeSeq)) {
            throw new UpdatesOutOfOrderException(true, this.purgeSeq, matchSeq, purgeSeq);
        }
    }

    protected final void incrementPurgeSeq(final long matchSeq, final long purgeSeq) throws IOException {
        assert Thread.holdsLock(this);
        assertPurgeSeqProgress(matchSeq, purgeSeq);
        this.purgeSeq = purgeSeq;
    }

    protected final void assertMinSeqs(final long minUpdateSeq, final long minPurgeSeq) throws StaleIndexException {
        if (this.updateSeq < minUpdateSeq) {
            throw new StaleIndexException(false, minUpdateSeq, this.updateSeq);
        }
        if (this.purgeSeq < minPurgeSeq) {
            throw new StaleIndexException(true, minPurgeSeq, this.purgeSeq);
        }
    }
}
