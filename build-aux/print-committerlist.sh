#!/bin/sh
#
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

function get_contributors {
  local OS=`uname -s`
  case "$OS" in
  Linux|CYGWIN*) # GNU sed
    local SED_ERE_FLAG=-r
  ;;
  *) # BSD sed
    local SED_ERE_FLAG=-E
  ;;
  esac

  local CONTRIB_EMAIL_SED_COMMAND="s/^[[:blank:]]{5}[[:digit:]]+[[:blank:]]/ * /"
  if [ "$1" == "couchdb-main-repo" ]
  then
    git shortlog -se 6c976bd..HEAD \
      | grep -v @apache.org \
      | sed $SED_ERE_FLAG -e "$CONTRIB_EMAIL_SED_COMMAND"
  else
    cd src/$1
    git shortlog -se HEAD \
      | grep -v @apache.org \
      | sed $SED_ERE_FLAG -e "$CONTRIB_EMAIL_SED_COMMAND"
    cd .. && cd ..
  fi
}

function print_comitter_list {
  # list of external repos that we exclude
  local EXCLUDE=("bear" "folsom" "goldrush" "ibrowse" "jiffy" "lager" "meck" "mochiweb" "snappy")
  local EXCLUDE=$(printf "\|%s" "${EXCLUDE[@]}")
  local EXCLUDE=${EXCLUDE:2}
  local SUBREPOS=$(ls src/ | grep -v "$EXCLUDE")

  if test -e .git; then

    {
      for i in $SUBREPOS; do
        get_contributors $i
      done;
      get_contributors "couchdb-main-repo"
    } | git check-mailmap --stdin | awk '
      BEGIN {
      }
      {
        $1 = "";
        persons[$0] = $0;
      }
      END {
        for (i in persons) {
          print persons[i];
        }
      }'
  fi
}
