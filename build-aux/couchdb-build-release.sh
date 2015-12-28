#!/bin/sh -e

VERSION=$1

if [ -z "$VERSION" ]; then
  echo "NO VERSION"
  exit 1
fi

echo "Building Apache CouchDB $VERSION"

RELDIR=apache-couchdb-$VERSION
# make release dir
rm -rf $RELDIR
mkdir $RELDIR

CURRENT_BRANCH=`git rev-parse --abbrev-ref HEAD`

# copy sources over
git archive $CURRENT_BRANCH | tar -xC $RELDIR/ -f -
mkdir $RELDIR/src
cd src/

for repo in *; do
  cd $repo
  mkdir ../../$RELDIR/src/$repo
  git_ish=`git rev-parse --short HEAD`
  git archive $git_ish | tar -xC ../../$RELDIR/src/$repo/ -f -
  set +e
  grep -rl '{vsn, git}' ../../$RELDIR/src/$repo/ | xargs sed -ie "s/{vsn, git}/{vsn, \"`git describe --always --tags`\"}/" 2> /dev/null
  set -e
  cd ..
done

cd ..

# create CONTRIBUTORS file
if test -e .git; then
    OS=`uname -s`
    case "$OS" in
    Linux|CYGWIN*) # GNU sed
        SED_ERE_FLAG=-r
    ;;
    *) # BSD sed
        SED_ERE_FLAG=-E
    ;;
    esac

    sed -e "/^#.*/d" CONTRIBUTORS.in > $RELDIR/CONTRIBUTORS
    CONTRIB_EMAIL_SED_COMMAND="s/^[[:blank:]]{5}[[:digit:]]+[[:blank:]]/ * /"
    git shortlog -se 6c976bd..HEAD \
        | grep -v @apache.org \
        | sed $SED_ERE_FLAG -e "$CONTRIB_EMAIL_SED_COMMAND" >> $RELDIR/CONTRIBUTORS
    echo "" >> $RELDIR/CONTRIBUTORS # simplest portable newline
    echo "For a list of authors see the \`AUTHORS\` file." >> $RELDIR/CONTRIBUTORS
fi
