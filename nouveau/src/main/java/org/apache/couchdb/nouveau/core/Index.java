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
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
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
    private boolean deleteOnClose = false;
    private long lastCommit = now();
    private volatile boolean closed;
    private final Semaphore permits = new Semaphore(Integer.MAX_VALUE);

    protected Index(final long updateSeq) {
        this.updateSeq = updateSeq;
    }

    public final boolean tryAcquire() {
        if (permits.tryAcquire() == false) {
            return false;
        }
        if (closed) {
            permits.release();
            return false;
        }
        return true;
    }

    public final boolean tryAcquire(long timeout, TimeUnit unit) throws InterruptedException {
        if (permits.tryAcquire(timeout, unit) == false) {
            return false;
        }
        if (closed) {
            permits.release();
            return false;
        }
        return true;
    }

    public final void release() {
        permits.release();
    }

    public final IndexInfo info() throws IOException {
        final int numDocs = doNumDocs();
        final long diskSize = doDiskSize();
        return new IndexInfo(updateSeq, numDocs, diskSize);
    }

    protected abstract int doNumDocs() throws IOException;

    protected abstract long doDiskSize() throws IOException;

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
        final boolean result = doCommit(updateSeq);
        if (result) {
            final long now = now();
            synchronized (this) {
                this.lastCommit = now;
            }
        }
        return result;
    }

    protected abstract boolean doCommit(final long updateSeq) throws IOException;

    @Override
    public final void close() throws IOException {
        synchronized (this) {
            closed = true;
        }
        // Ensures exclusive access to the index before closing.
        permits.acquireUninterruptibly(Integer.MAX_VALUE);
        try {
            doClose();
        } finally {
            permits.release(Integer.MAX_VALUE);
        }
    }

    protected abstract void doClose() throws IOException;

    public boolean isDeleteOnClose() {
        return deleteOnClose;
    }

    public void setDeleteOnClose(final boolean deleteOnClose) {
        synchronized (this) {
            this.deleteOnClose = deleteOnClose;
        }
    }

    protected final void assertUpdateSeqIsLower(final long updateSeq) throws UpdatesOutOfOrderException {
        assert Thread.holdsLock(this);
        if (!(updateSeq > this.updateSeq)) {
            throw new UpdatesOutOfOrderException(this.updateSeq, updateSeq);
        }
    }

    protected final void incrementUpdateSeq(final long updateSeq) throws IOException {
        assert Thread.holdsLock(this);
        assertUpdateSeqIsLower(updateSeq);
        this.updateSeq = updateSeq;
    }

    public boolean needsCommit(final long duration, final TimeUnit unit) {
        final long commitNeededSince = now() - unit.toNanos(duration);
        synchronized (this) {
            return this.lastCommit < commitNeededSince;
        }
    }

    private long now() {
        return System.nanoTime();
    }
}
