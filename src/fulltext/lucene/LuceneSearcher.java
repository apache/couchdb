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

LuceneSearcher searches a lucene index.

It is managed by the Apache CouchDB daemon.

*/

//basics
import java.io.*;

//lucene
import org.apache.lucene.index.Term;
import org.apache.lucene.index.IndexReader;

import org.apache.lucene.document.Document;

import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Hits;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.Query;

/*
protocol:
Queries will look like this:

databasename\n
the full text query\n

Then the java reader will read the lines and respond
by outputing each document result:
ok\n
docid1\n
score1\n
docid2\n
score2\n
docid3\n
score3\n
\n

or:

error\n
error_id\n
error message\n

*/
public class LuceneSearcher
{
    public static void main(String[] args) throws Exception
    {

        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

        String db = "";
        String queryString = "";

        while(((db = in.readLine()) != null) && ((queryString = in.readLine()) != null)) {

            IndexSearcher searcher = new IndexSearcher("Lucene/Index/" + db);

            Query query = new TermQuery(new Term("__couchdb_keywords", queryString));

            Hits hits = searcher.search(query);

            System.out.println("ok");
            for(int i = 0; i < hits.length(); i++) {
                Document d = hits.doc(i);
                System.out.println(d.get("__couchdb_document_id"));
                System.out.println(hits.score(i));
            }
            System.out.println();
        }
    }
}
