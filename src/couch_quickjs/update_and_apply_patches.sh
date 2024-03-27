#!/bin/bash

# Update from upstream master and apply patches in patches directory in order

set -e

# This is the main branch of the github mirror
URL=https://github.com/bellard/quickjs/archive/refs/heads/master.zip
#
# The other alternatives:
#   https://fuchsia.googlesource.com/third_party/quickjs
#   https://github.com/quickjs-ng/quickjs/commits/master/


echo
echo " * backup quickjs to quickjs.bak"
rm -rf quickjs.bak
mv quickjs quickjs.bak
echo
echo " * wget upstream ${URL}"
rm -rf master.zip
wget -q ${URL}
echo
echo " * unzip to quickjs"
unzip -q -o master.zip
rm master.zip
mv quickjs-master quickjs
echo
echo " * remove a few files we don't care about"
rm -rf quickjs/doc
rm -rf quickjs/examples
rm -rf quickjs/*test262o*
rm -f quickjs/tests/*.js
rm -f quickjs/tests/*.c
rm -f quickjs/.gitignore
rm quickjs/release.sh
rm quickjs/readme.txt
rm quickjs/unicode_gen.c
rm quickjs/unicode_gen_def.h
rm quickjs/unicode_download.sh
rm quickjs/repl.js
rm quickjs/qjs.c
rm quickjs/qjscalc.js
rm quickjs/TODO
echo
echo " * apply patches"
find patches -name "*.patch" -print0 | sort -z | xargs -t -0 -n 1 patch -p0 -i
echo
echo " * removing quickjs.bak"
rm -rf quickjs.bak
echo

# Example how to generate patches:
#
#  diff -u quickjs-master/quickjs.c quickjs/quickjs.c > patches/01-spidermonkey-185-mode.patch
