/*

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License.

*/

/*

LuceneIndexer creates a lucene index by incrementally fetching changes from a a
Apache CouchDB server. It is managed by the Apache CouchDB daemon.

I know this is Java and there should be a lot of OO going on, but it
isn't. Sorry about that.

*/

//basics
import java.io.*;
import java.net.*;
import java.util.*;
import java.nio.channels.FileChannel;
import java.nio.ByteBuffer;
import java.lang.reflect.*;


//couchdb4j
//import com.fourspaces.couchdb.*;

//xml
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import javax.xml.parsers.*;

//lucene
import org.apache.lucene.index.Term;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexReader;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.SimpleAnalyzer;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;

import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.Hits;
import org.apache.lucene.search.TermQuery;

public class LuceneIndexer
{
    private static CouchConfig configuration;
    private static com.fourspaces.couchdb.Session s;

    public static void main(String[] args) throws Exception
    {
/*      BufferedWriter out = new BufferedWriter(new FileWriter("LuceneIndexer.log"));
        out.write("indexer started");out.flush();
*/
        String db;
/*      out.write("indexer about to read config");out.flush();*/
        connect();
        readConfig();

/*      out.write("indexer read config: " + configuration.getDocuments());out.flush();*/

        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        try {
            while((db = in.readLine()) != null) {
/*              out.write("indexer got a poke");out.flush();*/

                if(db.equals("couchdbfulltext")) {
/*                  System.out.println("refresh config");

*/                  readConfig();
/*                  out.write("indexer refreshed config");out.flush();*/

                }

/*              out.write("indexer has table: " + db + "?");*/

                if(!configuration.hasDb(db)) {
/*                  out.write("... no wait for input");out.flush();*/

                    continue;
                }

/*              out.write("yeppa");out.flush();*/


                createIndexDir(db);
                indexChanges(db);
/*              System.out.println(db + " to revision: " + revision);*/
            }
        } catch (IOException e) {
/*          out.write("indexer caught IO exception: " + e.getMessage());out.flush();*/

        }
/*      System.out.println("Lucene Indexer stopped");*/
/*      out.write("indexer stopped");out.flush();*/

/*      out.close();*/

    }

    public static void connect() throws Exception
    {
        s = null;
        com.fourspaces.couchdb.Session s = new com.fourspaces.couchdb.Session("locahost", 5984);
    }

    public static void readConfig() throws Exception
    {
        //get all docs in /$ftconfig
        //return array of config docs
        configuration = null;
        configuration = new CouchConfig();
        com.fourspaces.couchdb.Database db = s.getDatabase("couchdbfulltext");
        com.fourspaces.couchdb.ViewResults changedDocuments = db.getAllDocuments(0);

        for (com.fourspaces.couchdb.Document d: changedDocuments.getResults()) {
            configuration.addDocument(d);
        }

/*      for(int i = 0; i < changedDocuments.length; i++) {
            CouchDocument document = changedDocuments[i];
            document = loadDocumentData(document, "couchdbfulltext");
            configuration.addDocument(document);
        }
*/  }

    public static void indexChanges(String db) throws Exception
    {
//      System.out.println("Updating index for '" + db + "' from revision: " + revision);
        int sequence = -1;
        try {
            com.fourspaces.couchdb.Database _db = s.getDatabase(db);
            sequence = _db.getUpdateSeq();
            com.fourspaces.couchdb.ViewResults changedDocuments = _db.getAllDocuments(sequence);

            if(changedDocuments.size() == 0) {
//              System.out.println("Index is up-to date at sequence_id: " + revision);
                return;
            }

            boolean delete = false;

            for (com.fourspaces.couchdb.Document d: changedDocuments.getResults()) {
                delete = d.getBoolean("delete");
                documentAddToIndex(db, d, delete);
            }
/*          for(int idx = 0; idx < changedDocuments.length; idx++) {
                com.fourspaces.couchdb.Document document = changedDocuments[idx];
                sequence = document.getUpdateSequence();
                delete = document.getDelete();
//              System.out.println("Doing: " + document + " with squence: " + sequence + " delete: "+document.getDelete() + " hash code:" + document.hashCode());

                document = loadDocumentData(document, db);
    //          System.out.println(changedDocuments[idx]);
                // remove from lucene if exists, add to lucene.

                documentAddToIndex(db, document, delete);
            }
*/      //  CouchDocument document = getDocumentByRevision(db, revision);
            setRevisionForDb(db, sequence);
        } catch(Exception e) {
//          System.out.println("Warning: " + db + " says: " + e.getMessage());
        }
    }

    public static void documentAddToIndex(String db, com.fourspaces.couchdb.Document document, boolean delete) throws IOException
    {
        String index = "Lucene/Index/" + db;
        boolean create = true;

/*      System.out.println("DEBUG: delete: " + delete);*/
/*      System.out.println("DEBUG: create index? " + create);*/

        if(IndexReader.indexExists(index)) {
            create = false;
            Term term = new Term("__couchdb_document_id", document.getId());
/*          System.out.println("DEBUG: Deleting: " + document + " with term:" + term);*/
            IndexReader reader = IndexReader.open(index);
            reader.deleteDocuments(term);
/*          System.out.println("DEBUG: reader has deletions: " + reader.hasDeletions());*/

            reader.close();
        }

        if(!delete) {
            Analyzer analyzer = new SimpleAnalyzer();

            IndexWriter writer = new IndexWriter(index, analyzer, create);
            writer.setUseCompoundFile(true);

/*          Collection fields = document.keys();*/
            Document luceneDocument = new Document();

/*          Set tmpKeys = fields.keySet();
            Object keys[] = tmpKeys.toArray();
*/          String keywords = "";

            for (Iterator it = document.keys(); it.hasNext(); ) {
                Object key = it.next();
                String value = document.getString((String)key);

                if(key.equals("__couchdb_document_id") || key.equals("__couchdb_document_revision")) {
                        luceneDocument.add(new Field((String)key, value, Field.Store.YES, Field.Index.UN_TOKENIZED));
                } else {
                    luceneDocument.add(new Field((String)key, value, Field.Store.YES, Field.Index.TOKENIZED));
                    keywords = keywords + " " + value;
                }
            }
            if(keywords.length() > 0) {
                luceneDocument.add(new Field("__couchdb_keywords", keywords, Field.Store.YES, Field.Index.TOKENIZED));
            }


/*          for(int idx = 0; idx < keys.length; idx++) {
    //          System.out.println("DEBUG: Add Field: "+ keys[idx] + " with value: " + fields.get(keys[idx]));
                Hashtable field = (Hashtable)fields.get(keys[idx]);
                if(field == null) {return;}
                for(int fieldIdx = 0; fieldIdx < field.size(); fieldIdx++) {
                    String value = (String)field.get(fieldIdx);
                    if(value == null) {
                        value = "";
                    }
    //              System.out.println("DEBUG: fieldIdx:" + fieldIdx + " and value: "+ value);
                    String key = (String)keys[idx];
                    if(key.equals("__couchdb_document_id") || key.equals("__couchdb_document_revision")) {
                        luceneDocument.add(new Field(key, value, Field.Store.YES, Field.Index.UN_TOKENIZED));
                    } else {
                        luceneDocument.add(new Field(key, value, Field.Store.YES, Field.Index.TOKENIZED));
                        keywords = keywords + " " + value;
                    }
                }
*///            }
            writer.addDocument(luceneDocument);
            writer.optimize();
            writer.close();
        }
    }


    private static void setRevisionForDb(String db, long revision) throws Exception
    {
        File dbFile = new File("Lucene/State/" + db);

        RandomAccessFile stateFile = new RandomAccessFile("Lucene/State/" + db, "rwd");
        stateFile.writeBytes(String.valueOf(revision));
        return;
    }

    private static String[] getDBs()
    {
        File dbRoot = new File("db_root");
        if(!dbRoot.isDirectory()) {
            return new String[0];
        }

        String[] dbs = dbRoot.list(new CouchDbDirFilter());

        return dbs;
    }

    private static long getRevisionForDb(String db) throws Exception
    {

        File dbFile = new File("Lucene/State/" + db);
        if(!dbFile.exists()) {
            return 0;
        }


        RandomAccessFile stateFile = new RandomAccessFile("Lucene/State/" + db, "r");
        String revision = stateFile.readLine();
//      System.out.println("rev: " + revision);
        return (long)Integer.parseInt(revision);
    }

    private static void createIndexDir(String db)
    {
        File indexDir = new File("Lucene/Index/" + db);
        if(!indexDir.exists()) {
            indexDir.mkdirs();
            System.out.println("Created Index Directory");
        }

        File stateDir = new File("Lucene/State");
        if(!stateDir.exists()) {
            stateDir.mkdirs();
            System.out.println("Created State Directory");
        }
    }

    private static XMLReader getParser(SAXCouchDocumentBuilder documentBuilder) throws Exception
    {
        SAXParserFactory factory = SAXParserFactory.newInstance();
        SAXParser saxParser = factory.newSAXParser();
        XMLReader parser = saxParser.getXMLReader();
        parser.setContentHandler(documentBuilder);
        return parser;
    }

    private static BufferedInputStream getUrlStream(String address) throws Exception
    {
        URL url = new URL(address);
        InputStream inStream = url.openStream();
        return new BufferedInputStream(inStream);
    }

    public static com.fourspaces.couchdb.ViewResults getChangedDocumentsSinceRevision(String db, int revision) throws Exception
    {
        //BufferedInputStream inBuffer = getUrlStream("http://localhost:5984/" + db + "/_all_docs_by_update_seq?startkey=" + revision);

        com.fourspaces.couchdb.ViewResults newDocs = s.getDatabase(db).getAllDocuments(revision);

        return newDocs;
        //return CouchDocument[]

/*      CouchDocument[] returnValue = {};
*/      //setup xml parser
/*      SAXCouchDocumentBuilder documentBuilder = new SAXCouchDocumentBuilder();
        XMLReader parser = getParser(documentBuilder);
        // Repeat until end of file
        parser.parse(new InputSource(inBuffer));


        return documentBuilder.getDocuments();
*/  }


    public static CouchDocument loadDocumentData(CouchDocument document, String db) throws Exception
    {
        BufferedInputStream inBuffer = getUrlStream("http://localhost:5984/" + db + "/" + document.getDocId() + "?rev=" + document.getRevision());

        //setup xml parser
        SAXCouchDocumentBuilder documentBuilder = new SAXCouchDocumentBuilder();
        XMLReader parser = getParser(documentBuilder);

        // Repeat until end of file
        parser.parse(new InputSource(inBuffer));

        return documentBuilder.getDocument();
    }
}
