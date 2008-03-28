This is still work in progress and has not been integrated into the build
process. Good luck though :)

This document describes how to use the LuceneIndexer with Apache CouchDB.

Requirements:
Apache CouchDB 0.6.4 or newer.
Java Development Kit (JDK) 1.5
Lucene 2.0.0 or newer
couchdb4j (http://code.google.com/p/couchdb4j/)


If you don't already have it,
download lucene-core-2.0.0.jar from a mirror
A list of mirrors can be found at
http://www.apache.org/dyn/closer.cgi/lucene/java/

Add the following line to your couch.ini:
LuceneServer=/usr/bin/java -cp "./bin/:./lib/lucene-core.jar" LuceneIndexer=...

Adjust the version number and the path to java, if needed.
If you have lucene installed already, remove the
'-cp "./bin/:./Lucene/lucene-core-2.0.0.jar"' part.

Put lucene-core.jar and cocuhdb4j.jar into $CouchDbDir/lib

Launch Apache CouchDB.

The indexer will populate $CouchDbDir/Lucene/Index with an index for
all documents in all databases.
(indexes per database will be added soon).

To see that the data is actually stored in there,
use luke from http://www.getopt.org/luke/

To use the actual index, you could use the PHP 5 Lucene Demo in the Zend Framework
(http://framework.zend.com) or any other Lucene implementation in your favourite
language.

If you have any questions, please visit:
http://couchdb.com/CouchDB/CouchDBWeb.nsf/vDissByDate
