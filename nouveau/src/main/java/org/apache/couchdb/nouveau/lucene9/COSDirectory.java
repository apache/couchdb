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

import com.ibm.cloud.objectstorage.AmazonServiceException;
import com.ibm.cloud.objectstorage.services.s3.AmazonS3;
import com.ibm.cloud.objectstorage.services.s3.model.AbortMultipartUploadRequest;
import com.ibm.cloud.objectstorage.services.s3.model.CompleteMultipartUploadRequest;
import com.ibm.cloud.objectstorage.services.s3.model.GetObjectRequest;
import com.ibm.cloud.objectstorage.services.s3.model.InitiateMultipartUploadRequest;
import com.ibm.cloud.objectstorage.services.s3.model.ListMultipartUploadsRequest;
import com.ibm.cloud.objectstorage.services.s3.model.ListObjectsV2Request;
import com.ibm.cloud.objectstorage.services.s3.model.ListObjectsV2Result;
import com.ibm.cloud.objectstorage.services.s3.model.MultipartUploadListing;
import com.ibm.cloud.objectstorage.services.s3.model.PartETag;
import com.ibm.cloud.objectstorage.services.s3.model.UploadPartRequest;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.zip.CRC32;
import java.util.zip.CheckedOutputStream;
import org.apache.lucene.store.BaseDirectory;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.IOContext;
import org.apache.lucene.store.IndexInput;
import org.apache.lucene.store.IndexOutput;
import org.apache.lucene.store.Lock;
import org.apache.lucene.store.LockFactory;
import org.apache.lucene.store.LockObtainFailedException;
import org.apache.lucene.store.LockReleaseFailedException;

public final class COSDirectory extends BaseDirectory {

    private static final LockFactory COS_LOCK_FACTORY = new COSLockFactory();

    private static final int READ_AHEAD_BUFFER_SIZE = 128 * 1024;

    private static final int WRITE_BUFFER_SIZE = 1024 * 1024;

    private static final String LEGAL_HOLD_ID = "COSDirectoryLock";

    private final AmazonS3 s3;

    private final String bucketName;

    private final String prefix;

    public COSDirectory(final AmazonS3 s3, final String bucketName, final String prefix) throws IOException {
        this(s3, bucketName, prefix, COS_LOCK_FACTORY);
    }

    public COSDirectory(final AmazonS3 s3, final String bucketName, final String prefix, LockFactory lockFactory)
            throws IOException {
        super(lockFactory);
        this.s3 = Objects.requireNonNull(s3);
        this.bucketName = Objects.requireNonNull(bucketName);
        this.prefix = Objects.requireNonNull(prefix);
    }

    @Override
    public String[] listAll() throws IOException {
        final int strip = prefix.length() + 1;
        try {
            final List<String> result = new LinkedList<String>();
            String continuationToken = "";
            ListObjectsV2Result listObjectsResult;
            do {
                var request = new ListObjectsV2Request()
                        .withBucketName(bucketName)
                        .withPrefix(prefix)
                        .withContinuationToken(continuationToken);
                listObjectsResult = s3.listObjectsV2(request);
                for (var s : listObjectsResult.getObjectSummaries()) {
                    result.add(s.getKey().substring(strip));
                }
                continuationToken = listObjectsResult.getContinuationToken();
            } while (listObjectsResult.isTruncated());
            return result.toArray(String[]::new);
        } catch (final AmazonServiceException e) {
            throw new IOException(String.format("listAll for %s/%s failed", bucketName, prefix), e);
        }
    }

    @Override
    public void deleteFile(String name) throws IOException {
        try {
            s3.deleteObject(bucketName, key(name));
        } catch (final AmazonServiceException e) {
            throw new IOException(String.format("deleteFile for %s/%s failed", bucketName, key(name)), e);
        }
    }

    @Override
    public long fileLength(String name) throws IOException {
        return getObjectLength(key(name));
    }

    @Override
    public IndexOutput createOutput(String name, IOContext context) throws IOException {
        return new COSIndexOutput(name);
    }

    @Override
    public IndexOutput createTempOutput(String prefix, String suffix, IOContext context) throws IOException {
        final var name = String.format("%s-%s-%s", prefix, UUID.randomUUID(), suffix);
        return createOutput(name, context);
    }

    @Override
    public void sync(Collection<String> names) throws IOException {
        // no-op
    }

    @Override
    public void syncMetaData() throws IOException {
        // no-op
    }

    @Override
    public void rename(String source, String dest) throws IOException {
        try {
            s3.copyObject(bucketName, key(source), bucketName, key(dest));
            s3.deleteObject(bucketName, key(source));
        } catch (final AmazonServiceException e) {
            throw new IOException(
                    String.format("Rename of %s/%s to %s/%s failed", bucketName, key(source), bucketName, key(dest)),
                    e);
        }
    }

    @Override
    public IndexInput openInput(String name, IOContext context) throws IOException {
        final var length = getObjectLength(key(name));
        return new COSIndexInput(name, 0, length);
    }

    @Override
    public void close() throws IOException {
        // no-op
    }

    @Override
    public Set<String> getPendingDeletions() throws IOException {
        return Collections.emptySet();
    }

    public void cleanupMultipartUploads() throws IOException {
        var request = new ListMultipartUploadsRequest(bucketName);
        try {
            MultipartUploadListing result;
            do {
                result = s3.listMultipartUploads(request);
                for (var part : result.getMultipartUploads()) {
                    if (part.getKey().startsWith(prefix)) {
                        s3.abortMultipartUpload(
                                new AbortMultipartUploadRequest(bucketName, part.getKey(), part.getUploadId()));
                    }
                }
                request.setKeyMarker(result.getKeyMarker());
                request.setUploadIdMarker(result.getUploadIdMarker());
            } while (result.isTruncated());
        } catch (final AmazonServiceException e) {
            throw new IOException(String.format("Failed to cleanup multipart uploads in %s/%s", bucketName, prefix), e);
        }
    }

    private String key(final String name) {
        return String.format("%s/%s", prefix, name);
    }

    private class COSIndexInput extends IndexInput {

        private final String name;

        private final long offset;

        private final long length;

        private long pos;

        private byte[] buffer;

        private long bufferOffset;

        private COSIndexInput(final String name, final long offset, final long length) {
            super(String.format("cos://%s/%s/%s", bucketName, prefix, name));
            this.name = Objects.requireNonNull(name);
            this.offset = offset;
            this.length = length;
        }

        @Override
        public void close() throws IOException {
            // no-op
        }

        @Override
        public COSIndexInput clone() {
            final COSIndexInput result = new COSIndexInput(name, offset, length);
            result.pos = pos;
            return result;
        }

        @Override
        public long getFilePointer() {
            return pos;
        }

        @Override
        public void seek(long pos) throws IOException {
            if (pos > length) {
                throw new EOFException();
            }
            this.pos = pos;
        }

        @Override
        public long length() {
            return length;
        }

        @Override
        public IndexInput slice(String sliceDescription, long offset, long length) throws IOException {
            return new COSIndexInput(name, offset, length);
        }

        @Override
        public byte readByte() throws IOException {
            readAhead(1);
            this.pos++;
            return buffer[(int) (this.offset + this.pos - 1 - bufferOffset)];
        }

        @Override
        public void readBytes(final byte[] b, final int offset, final int len) throws IOException {
            readAhead(len);
            System.arraycopy(buffer, (int) (this.offset + this.pos - bufferOffset), b, offset, len);
            this.pos += len;
        }

        private void readAhead(final int min) throws IOException {
            // Ensure we have buffered at least offset + pos to offset + pos + min from COS.
            // To save roundtrips read ahead a full BUFFER_SIZE unless min is even larger.
            if (buffer == null
                    || !(this.offset + this.pos >= this.bufferOffset
                            && this.offset + this.pos + min <= this.bufferOffset + this.buffer.length)) {
                final var request = new GetObjectRequest(bucketName, key(name));
                request.setRange(
                        this.offset + this.pos, this.offset + this.pos + Math.max(min, READ_AHEAD_BUFFER_SIZE));
                try {
                    final var object = s3.getObject(request);
                    try (var in = object.getObjectContent()) {
                        this.buffer = in.readAllBytes();
                        this.bufferOffset = this.offset + this.pos;
                    }
                } catch (final AmazonServiceException e) {
                    throw new IOException(String.format("readByte failed for %s/%s", bucketName, key(name)), e);
                }
            }
        }
    }

    private long getObjectLength(final String key) throws IOException {
        try {
            return s3.getObjectMetadata(bucketName, key).getContentLength();
        } catch (final AmazonServiceException e) {
            throw new IOException(String.format("getObjectLength failed for %s/%s", bucketName, key), e);
        }
    }

    private static class COSLockFactory extends LockFactory {

        @Override
        public Lock obtainLock(Directory dir, String lockName) throws IOException {
            if (!(dir instanceof COSDirectory)) {
                throw new IllegalArgumentException(dir + " not supported by this lock factory");
            }
            final var cosDir = (COSDirectory) dir;
            try {
                cosDir.s3.addLegalHold(cosDir.bucketName, cosDir.key(lockName), LEGAL_HOLD_ID);
            } catch (final AmazonServiceException e) {
                throw new LockObtainFailedException(
                        String.format("Failed to lock %s/%s", cosDir.bucketName, cosDir.key(lockName)), e);
            }
            cosDir.cleanupMultipartUploads();
            return new COSLock(cosDir, lockName);
        }
    }

    private class COSIndexOutput extends IndexOutput {

        private final String name;

        private final String uploadId;

        private final List<PartETag> partETags;

        private final CRC32 crc;

        private final ByteArrayOutputStream bos;

        private final CheckedOutputStream cos;

        private int partNumber;

        private long bytesWritten;

        private boolean failed;

        private COSIndexOutput(final String name) throws IOException {
            super(String.format("cos://%s/%s/%s", bucketName, prefix, name), name);

            final var request = new InitiateMultipartUploadRequest(bucketName, key(name));
            try {
                final var result = s3.initiateMultipartUpload(request);
                this.uploadId = result.getUploadId();
            } catch (final AmazonServiceException e) {
                throw new IOException(
                        String.format("InitiateMultipartUpload failed for %s/%s", bucketName, key(name)), e);
            }

            this.name = Objects.requireNonNull(name);
            this.partETags = new LinkedList<PartETag>();
            this.crc = new CRC32();
            this.bos = new ByteArrayOutputStream(WRITE_BUFFER_SIZE);
            this.cos = new CheckedOutputStream(bos, crc);
            this.partNumber = 1;
        }

        @Override
        public void close() throws IOException {
            cos.close();

            if (failed) {
                try {
                    s3.abortMultipartUpload(new AbortMultipartUploadRequest(bucketName, key(name), uploadId));
                } catch (final AmazonServiceException e) {
                    throw new IOException(
                            String.format("abortMultipartUpload failed for %s/%s", bucketName, key(name)), e);
                }
            } else {
                flush(true);
                final var request = new CompleteMultipartUploadRequest(bucketName, key(name), uploadId, partETags);
                try {
                    s3.completeMultipartUpload(request);
                } catch (final AmazonServiceException e) {
                    throw new IOException(
                            String.format("completeMultipartUpload failed for %s/%s", bucketName, key(name)), e);
                }
            }
        }

        @Override
        public long getFilePointer() {
            return bytesWritten;
        }

        @Override
        public long getChecksum() throws IOException {
            return crc.getValue();
        }

        @Override
        public void writeByte(final byte b) throws IOException {
            assert bos.size() < WRITE_BUFFER_SIZE : "buffer should never be full at start of method";
            cos.write(b);
            bytesWritten++;
            flush(false);
        }

        @Override
        public void writeBytes(final byte[] b, int offset, int length) throws IOException {
            assert bos.size() < WRITE_BUFFER_SIZE : "buffer should never be full at start of method";

            while (length > 0) {
                var l = (int) Math.min(WRITE_BUFFER_SIZE - bos.size(), length);
                cos.write(b, offset, l);
                bytesWritten += l;
                offset += l;
                length -= l;
                flush(false);
            }
        }

        private void flush(final boolean force) throws IOException {
            var bufferSize = bos.size();
            if (bufferSize == WRITE_BUFFER_SIZE || (force && bufferSize > 0)) {
                var bytes = bos.toByteArray();
                bos.reset();
                var request = new UploadPartRequest()
                        .withBucketName(bucketName)
                        .withKey(key(name))
                        .withUploadId(uploadId)
                        .withPartSize(bytes.length)
                        .withPartNumber(partNumber)
                        .withInputStream(new ByteArrayInputStream(bytes));
                try {
                    var result = s3.uploadPart(request);
                    this.partNumber++;
                    this.partETags.add(result.getPartETag());
                } catch (final AmazonServiceException e) {
                    failed = true;
                    throw new IOException(String.format("uploadPart failed for %s/%s", bucketName, key(name)), e);
                }
            }
        }
    }

    private static class COSLock extends Lock {

        private final COSDirectory cosDir;

        private final String lockName;

        private COSLock(final COSDirectory cosDir, final String lockName) {
            this.cosDir = Objects.requireNonNull(cosDir);
            this.lockName = Objects.requireNonNull(lockName);
        }

        @Override
        public void close() throws IOException {
            try {
                cosDir.s3.deleteLegalHold(cosDir.bucketName, cosDir.key(lockName), LEGAL_HOLD_ID);
            } catch (final AmazonServiceException e) {
                throw new LockReleaseFailedException(
                        String.format("Failed to release %s/%s", cosDir.bucketName, cosDir.key(lockName)), e);
            }
        }

        @Override
        public void ensureValid() throws IOException {
            try {
                final var holds = cosDir.s3.listLegalHolds(cosDir.bucketName, cosDir.key(lockName));
                final var isValid =
                        holds.getLegalHolds().stream().anyMatch(h -> h.getId().equals(LEGAL_HOLD_ID));
                if (!isValid) {
                    throw new IOException("Lock no longer held");
                }
            } catch (final AmazonServiceException e) {
                throw new IOException(e);
            }
        }
    }
}
