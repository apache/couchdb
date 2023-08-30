package org.apache.couchdb.nouveau.lucene9;

import com.ibm.cloud.objectstorage.ClientConfiguration;
import com.ibm.cloud.objectstorage.client.builder.AwsClientBuilder.EndpointConfiguration;
import com.ibm.cloud.objectstorage.services.s3.AmazonS3;
import com.ibm.cloud.objectstorage.services.s3.AmazonS3ClientBuilder;
import java.io.IOException;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.DoublePoint;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.NoLockFactory;
import org.junit.jupiter.api.Test;

public class COSDirectoryTest {

    @Test
    public void testBasics() throws Exception {
        AmazonS3 s3 = createClient("s3.eu-gb.cloud-object-storage.appdomain.cloud", "eu-gb");

        var cosDir = new COSDirectory(s3, "rnewson-cosdir-demo", "index1", NoLockFactory.INSTANCE);
        cleanup(cosDir);
        try {
            var conf = new IndexWriterConfig(new StandardAnalyzer());
            conf.setUseCompoundFile(false);

            try (var writer = new IndexWriter(cosDir, conf)) {
                var doc = new Document();
                doc.add(new TextField("foo", "hello there", Store.YES));
                doc.add(new StoredField("bar", "bar"));
                doc.add(new DoublePoint("baz", 12.0));
                writer.addDocument(doc);
                writer.commit();
            }
        } finally {
            cleanup(cosDir);
        }
    }

    private static AmazonS3 createClient(String endpointUrl, String location) {
        ClientConfiguration clientConfig = new ClientConfiguration()
                .withRequestTimeout(5000)
                .withMaxErrorRetry(0)
                .withMaxConnections(1);

        return AmazonS3ClientBuilder.standard()
                .withEndpointConfiguration(new EndpointConfiguration(endpointUrl, location))
                .withPathStyleAccessEnabled(true)
                .withClientConfiguration(clientConfig)
                .build();
    }

    private void cleanup(final Directory dir) throws IOException {
        for (String name : dir.listAll()) {
            dir.deleteFile(name);
        }
        if (dir instanceof COSDirectory) {
            ((COSDirectory) dir).cleanupMultipartUploads();
        }
    }
}
