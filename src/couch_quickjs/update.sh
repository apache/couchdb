#!/bin/bash

# Update from upstream master and apply patches in patches directory in order

set -e

# This is the main branch of the github mirror
URL=https://github.com/bellard/quickjs/archive/refs/heads/master.zip
#

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
rm quickjs/TODO
rm quickjs/readme-cosmo.txt
# Skip Temporal and Intl bits. We'll add them later perhaps
# rebar.config.script builds with CONFIG_TEMPORAL_INTL= (off)
rm -rf quickjs/intl
rm -rf quickjs/temporal
rm quickjs/gen-locale-data.js
rm quickjs/gen_tzdata.py

echo
echo " * skip temporal and intl support for now in test262.conf"
awk '
    /^(Temporal|Intl[-.]|intl-)/ && !/=/ { print $0 "=skip"; next }
    { print }
    /^\[exclude\]$/ { print "test262/test/intl402/" }
' quickjs/test262.conf > quickjs/test262.conf.tmp
mv quickjs/test262.conf.tmp quickjs/test262.conf

echo
echo " * apply patches"
find patches -name "*.patch" -print0 | sort -z | xargs -t -0 -n 1 patch -p0 -i
echo
echo " * removing quickjs.bak"
rm -rf quickjs.bak
echo

# Example how to update patches themselves:
#
# Run
#  ./update_patches.sh
# OR manually run after cloning and unzipping master.zip from quickjs:
#  diff -u quickjs-master/quickjs.c quickjs/quickjs.c > patches/01-spidermonkey-185-mode.patch
#  diff -u quickjs-master/test262_errors.txt quickjs/test262_errors.txt > patches/02-test262-errors.patch
#
# To run test262 from the quickjs directory use:
#  make CONFIG_TEMPORAL_INTL= test2-bootstrap
#  make CONFIG_TEMPORAL_INTL= test2
# and "make CONFIG_TEMPORAL_INTL= test2-update" to regenerate test262_errors.txt
