#!/bin/sh
rm -rf apache-couchdb*
./configure
make release
cd apache-couchdb
  ./configure
cd ..

cp -r apache-couchdb apache-couchdb-pristine

cd apache-couchdb
  make
  make clean
cd ..

echo "********************************************"
echo "If you see anything here"
diff -r apache-couchdb apache-couchdb-pristine
echo "and here, something is wrong"
echo "********************************************"
