#!/bin/sh
rm -rf apache-couchdb apache-couchdb-pristine
./configure
make release
cp -r apache-couchdb apache-couchdb-pristine
cd apache-couchdb
  ./configure
  make distclean
cd ..

echo "********************************************"
echo "If you see anything here"
diff -r apache-couchdb apache-couchdb-pristine
echo "and here, something is wrong"
echo "********************************************"
