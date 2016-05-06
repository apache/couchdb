#!/bin/sh
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# requires shunit2 to be in $PATH
# http://shunit2.googlecode.com/
# uses `checkbashisms` if in $PATH


SHUNIT2=`which shunit2`

if [ -z "$SHUNIT2" -o ! -x "$SHUNIT2" ]; then
    echo
    echo "Error: This test script requires the shunit2 script to be in \$PATH".
    echo "You can download shunit2 from http://shunit2.googlecode.com or via"
    echo "your preferred package manager."
    echo
    exit 1
fi

CHECKBASHISMS=`which checkbashisms`

if [ -n "$CHECKBASHISMS" -a -x "$CHECKBASHISMS" ]; then
    echo "Checking for bash-isms"

    echo "  in ./configure"
    `$CHECKBASHISMS -npfx configure`
    if [ $? -ne 0 ]; then
        echo "./configure includes bashisms, do not release"
    fi
    echo "  done"

    echo "  in ./build-aux/couchdb-build-release.sh"
    `$CHECKBASHISMS -npfx ./build-aux/couchdb-build-release.sh`
    if [ $? -ne 0 ]; then
        echo "./build-aux/couchdb-build-release.sh includes bashisms, do not release"
    fi
    echo "  done"
fi


# shunit2 tests
CMD="./configure --test "

test_defaults() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"
    RESULT=`$CMD`
    assertEquals "test defaults" "$EXPECT" "$RESULT"
}

test_prefix() {
    EXPECT="/opt/local /opt/local /opt/local/bin /opt/local/libexec /opt/local/etc /opt/local/share /opt/local/share /opt/local/var /opt/local/var/run /opt/local/share/doc/apache-couchdb /opt/local/lib /opt/local/var/lib/couchdb /opt/local/var/lib/couchdb /opt/local/var/log /opt/local/share/man /opt/local/share/info /opt/local/share/doc/apache-couchdb/html /opt/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --prefix=/opt/local`
    assertEquals "test prefix" "$EXPECT" "$RESULT"

    RESULT=`$CMD --prefix /opt/local`
    assertEquals "test prefix" "$EXPECT" "$RESULT"
}

test_prefix_error() {
    EXPECT='ERROR: "--prefix" requires a non-empty argument.'

    RESULT=`$CMD --prefix= 2>&1`
    assertEquals "test prefix error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --prefix 2>&1`
    assertEquals "test prefix error" "$EXPECT" "$RESULT"
}


test_exec_prefix() {
    EXPECT="/usr/local /opt/local /opt/local/bin /opt/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /opt/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --exec-prefix=/opt/local`
    assertEquals "test exec_prefix" "$EXPECT" "$RESULT"

    RESULT=`$CMD --exec-prefix /opt/local`
    assertEquals "test exec_prefix" "$EXPECT" "$RESULT"
}

test_exec_prefix_eval() {
    EXPECT="/horse/local /horse/local /horse/local/bin /horse/local/libexec /horse/local/etc /horse/local/share /horse/local/share /horse/local/var /horse/local/var/run /horse/local/share/doc/apache-couchdb /horse/local/lib /horse/local/var/lib/couchdb /horse/local/var/lib/couchdb /horse/local/var/log /horse/local/share/man /horse/local/share/info /horse/local/share/doc/apache-couchdb/html /horse/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --prefix=/horse/local --exec-prefix=\\${prefix}`
    assertEquals "test exec_prefix" "$EXPECT" "$RESULT"

    RESULT=`$CMD --prefix /horse/local --exec-prefix \\${prefix}`
    assertEquals "test exec_prefix" "$EXPECT" "$RESULT"
}

test_exec_prefix_error() {
    EXPECT='ERROR: "--exec-prefix" requires a non-empty argument.'

    RESULT=`$CMD --exec-prefix= 2>&1`
    assertEquals "test exec_prefix error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --exec-prefix 2>&1`
    assertEquals "test exec_prefix error" "$EXPECT" "$RESULT"
}

test_bindir() {
    EXPECT="/usr/local /usr/local /my/funky/bindir /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --bindir=/my/funky/bindir`
    assertEquals "test bindir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --bindir /my/funky/bindir`
    assertEquals "test bindir" "$EXPECT" "$RESULT"
}

test_bindir_error() {
    EXPECT='ERROR: "--bindir" requires a non-empty argument.'

    RESULT=`$CMD --bindir= 2>&1`
    assertEquals "test bindir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --bindir 2>&1`
    assertEquals "test bindir error" "$EXPECT" "$RESULT"
}

test_libexecdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /opt/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --libexecdir=/opt/local/libexec`
    assertEquals "test libexecdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --libexecdir /opt/local/libexec`
    assertEquals "test libexecdir" "$EXPECT" "$RESULT"
}

test_libexecdir_error() {
    EXPECT='ERROR: "--libexecdir" requires a non-empty argument.'

    RESULT=`$CMD --libexecdir= 2>&1`
    assertEquals "test libexecdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --libexecdir 2>&1`
    assertEquals "test libexecdir error" "$EXPECT" "$RESULT"
}

test_sysconfdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /opt/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --sysconfdir=/opt/local/etc`
    assertEquals "test sysconfdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --sysconfdir /opt/local/etc`
    assertEquals "test sysconfdir" "$EXPECT" "$RESULT"
}

test_sysconfdir_error() {
    EXPECT='ERROR: "--sysconfdir" requires a non-empty argument.'

    RESULT=`$CMD --sysconfdir= 2>&1`
    assertEquals "test sysconfdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --sysconfdir 2>&1`
    assertEquals "test sysconfdir error" "$EXPECT" "$RESULT"
}

test_datarootdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /opt/local/share /opt/local/share /usr/local/var /usr/local/var/run /opt/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /opt/local/share/man /opt/local/share/info /opt/local/share/doc/apache-couchdb/html /opt/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --datarootdir=/opt/local/share`
    assertEquals "test datarootdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --datarootdir /opt/local/share`
    assertEquals "test datarootdir" "$EXPECT" "$RESULT"
}

test_datarootdir_error() {
    EXPECT='ERROR: "--datarootdir" requires a non-empty argument.'

    RESULT=`$CMD --datarootdir= 2>&1`
    assertEquals "test datarootdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --datarootdir 2>&1`
    assertEquals "test datarootdir error" "$EXPECT" "$RESULT"
}

test_localstatedir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /horse/local/var /horse/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /horse/local/var/lib/couchdb /horse/local/var/lib/couchdb /horse/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --localstatedir=/horse/local/var`
    assertEquals "test localstatedir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --localstatedir /horse/local/var`
    assertEquals "test localstatedir" "$EXPECT" "$RESULT"
}

test_localstatedir_error() {
    EXPECT='ERROR: "--localstatedir" requires a non-empty argument.'

    RESULT=`$CMD --localstatedir= 2>&1`
    assertEquals "test localstatedir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --localstatedir 2>&1`
    assertEquals "test localstatedir error" "$EXPECT" "$RESULT"
}

test_runstatedir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /horse/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --runstatedir=/horse/local/var/run`
    assertEquals "test runstatedir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --runstatedir /horse/local/var/run`
    assertEquals "test runstatedir" "$EXPECT" "$RESULT"
}

test_runstatedir_error() {
    EXPECT='ERROR: "--runstatedir" requires a non-empty argument.'

    RESULT=`$CMD --runstatedir= 2>&1`
    assertEquals "test runstatedir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --runstatedir 2>&1`
    assertEquals "test runstatedir error" "$EXPECT" "$RESULT"
}

test_docdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /horse/local/share/doc /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /horse/local/share/doc/html /horse/local/share/doc/pdf"

    RESULT=`$CMD --docdir=/horse/local/share/doc`
    assertEquals "test docdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --docdir /horse/local/share/doc`
    assertEquals "test docdir" "$EXPECT" "$RESULT"
}

test_docdir_error() {
    EXPECT='ERROR: "--docdir" requires a non-empty argument.'

    RESULT=`$CMD --docdir= 2>&1`
    assertEquals "test docdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --docdir 2>&1`
    assertEquals "test docdir error" "$EXPECT" "$RESULT"
}

test_libdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /horse/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --libdir=/horse/local/lib`
    assertEquals "test libdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --libdir /horse/local/lib`
    assertEquals "test libdir" "$EXPECT" "$RESULT"
}

test_libdir_error() {
    EXPECT='ERROR: "--libdir" requires a non-empty argument.'

    RESULT=`$CMD --libdir= 2>&1`
    assertEquals "test libdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --libdir 2>&1`
    assertEquals "test libdir error" "$EXPECT" "$RESULT"
}

test_database_dir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /horse/local/var/lib /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --databasedir=/horse/local/var/lib`
    assertEquals "test databasedir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --databasedir /horse/local/var/lib`
    assertEquals "test databasedir" "$EXPECT" "$RESULT"
}

test_database_dir_error() {
    EXPECT='ERROR: "--databasedir" requires a non-empty argument.'

    RESULT=`$CMD --databasedir= 2>&1`
    assertEquals "test databasedir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --databasedir 2>&1`
    assertEquals "test databasedir error" "$EXPECT" "$RESULT"
}

test_view_dir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /horse/local/var/lib /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --viewindexdir=/horse/local/var/lib`
    assertEquals "test viewindexdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --viewindexdir /horse/local/var/lib`
    assertEquals "test viewindexdir" "$EXPECT" "$RESULT"
}

test_view_dir_error() {
    EXPECT='ERROR: "--viewindexdir" requires a non-empty argument.'

    RESULT=`$CMD --viewindexdir= 2>&1`
    assertEquals "test viewindexdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --viewindexdir 2>&1`
    assertEquals "test viewindexdir error" "$EXPECT" "$RESULT"
}

test_logdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /horse/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --logdir=/horse/log`
    assertEquals "test logdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --logdir /horse/log`
    assertEquals "test logdir" "$EXPECT" "$RESULT"
}

test_logdir_error() {
    EXPECT='ERROR: "--logdir" requires a non-empty argument.'

    RESULT=`$CMD --logdir= 2>&1`
    assertEquals "test logdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --logdir 2>&1`
    assertEquals "test logdir error" "$EXPECT" "$RESULT"
}

test_mandir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /horse/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --mandir=/horse/local/share/man`
    assertEquals "test mandir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --mandir /horse/local/share/man`
    assertEquals "test mandir" "$EXPECT" "$RESULT"
}

test_mandir_error() {
    EXPECT='ERROR: "--mandir" requires a non-empty argument.'

    RESULT=`$CMD --mandir= 2>&1`
    assertEquals "test mandir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --mandir 2>&1`
    assertEquals "test mandir error" "$EXPECT" "$RESULT"
}

test_infodir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /horse/local/share/info /usr/local/share/doc/apache-couchdb/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --infodir=/horse/local/share/info`
    assertEquals "test infodir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --infodir /horse/local/share/info`
    assertEquals "test infodir" "$EXPECT" "$RESULT"
}

test_infodir_error() {
    EXPECT='ERROR: "--infodir" requires a non-empty argument.'

    RESULT=`$CMD --infodir= 2>&1`
    assertEquals "test infodir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --infodir 2>&1`
    assertEquals "test infodir error" "$EXPECT" "$RESULT"
}

test_htmldir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /horse/local/share/doc/html /usr/local/share/doc/apache-couchdb/pdf"

    RESULT=`$CMD --htmldir=/horse/local/share/doc/html`
    assertEquals "test htmldir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --htmldir /horse/local/share/doc/html`
    assertEquals "test htmldir" "$EXPECT" "$RESULT"
}

test_htmldir_error() {
    EXPECT='ERROR: "--htmldir" requires a non-empty argument.'

    RESULT=`$CMD --htmldir= 2>&1`
    assertEquals "test htmldir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --htmldir 2>&1`
    assertEquals "test htmldir error" "$EXPECT" "$RESULT"
}

test_pdfdir() {
    EXPECT="/usr/local /usr/local /usr/local/bin /usr/local/libexec /usr/local/etc /usr/local/share /usr/local/share /usr/local/var /usr/local/var/run /usr/local/share/doc/apache-couchdb /usr/local/lib /usr/local/var/lib/couchdb /usr/local/var/lib/couchdb /usr/local/var/log /usr/local/share/man /usr/local/share/info /usr/local/share/doc/apache-couchdb/html /horse/local/share/doc/pdf"

    RESULT=`$CMD --pdfdir=/horse/local/share/doc/pdf`
    assertEquals "test pdfdir" "$EXPECT" "$RESULT"

    RESULT=`$CMD --pdfdir /horse/local/share/doc/pdf`
    assertEquals "test pdfdir" "$EXPECT" "$RESULT"
}

test_pdfdir_error() {
    EXPECT='ERROR: "--pdfdir" requires a non-empty argument.'

    RESULT=`$CMD --pdfdir= 2>&1`
    assertEquals "test pdfdir error" "$EXPECT" "$RESULT"

    RESULT=`$CMD --pdfdir 2>&1`
    assertEquals "test pdfdir error" "$EXPECT" "$RESULT"
}

# source the shunit2
. $SHUNIT2
