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

import java.util.*;


class CouchConfig
{
/*  private CouchDocument[] documents;
*/
    private Hashtable documents;
    private long updateSequence;

    public CouchConfig()
    {
        documents = new Hashtable();
        updateSequence = 0;
    }

    public void setUpdateSequence(long newUpdateSequence)
    {
        updateSequence = newUpdateSequence;
    }

    public long getUpdateSequence()
    {
        return updateSequence;
    }

    public void addDocument(com.fourspaces.couchdb.Document document)
    {
        String field;
//      System.out.println(document);
        field = document.getString("__couchdb_database");
//      System.out.println(field);
        if(field != null) {
            documents.put(field, document);
        }
    }

    public Hashtable getDocuments()
    {
        return documents;
    }

    public boolean hasDb(String db)
    {
        return documents.containsKey(db);
    }
}
