#!/bin/sh -e

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

SRCDIR="%abs_top_srcdir%"
BUILDDIR="%abs_top_builddir%"
export ERL_LIBS="$BUILDDIR/src/:$ERL_LIBS"
export ERL_FLAGS="$ERL_FLAGS -pa $BUILDDIR/test/etap/"

if test $# -eq 1; then
    OPTS=""
    TGT=$1
else
    OPTS=$1
    TGT=$2
fi

if test -f $TGT; then
    prove $OPTS $TGT
else
    prove $OPTS $TGT/*.t
fi
