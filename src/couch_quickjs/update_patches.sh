# Update patches
#
# Call this script after using update_and_apply_patches.sh to adjust
# the patches themselves. Sometimes line offsets drift and so this takes
# a new diff from the master QuickJS vs current source tree and regenerates
# the patch.

set -e

URL=https://github.com/bellard/quickjs/archive/refs/heads/master.zip

echo " * wget ${URL}"
rm -rf master.zip quickjs-master
wget -q ${URL}
echo " * unzip master.zip to quickjs-master"
unzip -q -o master.zip

set +e

echo " * updating 01-spidermonkey-185-mode.patch"
diff -u quickjs-master/quickjs.c quickjs/quickjs.c > patches/01-spidermonkey-185-mode.patch

echo " * updating 02-test262-errors.patch"
diff -u quickjs-master/test262_errors.txt quickjs/test262_errors.txt> patches/02-test262-errors.patch
set -e

echo " * cleaning up"
rm -rf master.zip quickjs-master
