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
git archive $CURRENT_BRANCH | tar -xC $RELDIR/
mkdir $RELDIR/src
cd src/

for repo in *; do
  cd $repo
  mkdir ../../$RELDIR/src/$repo
  git_ish=`git symbolic-ref -q --short HEAD || git describe --tags --exact-match`
  git archive $git_ish | tar -xC ../../$RELDIR/src/$repo/
  cd ..
done

cd ..

# update version
# actual version detection TBD
perl -pi -e "s/\{vsn, git\}/\{vsn, \"$VERSION\"\}/" $RELDIR/src/*/src/*.app.src

# create THANKS file
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

    sed -e "/^#.*/d" THANKS.in > $RELDIR/THANKS
    CONTRIB_EMAIL_SED_COMMAND="s/^[[:blank:]]{5}[[:digit:]]+[[:blank:]]/ * /"
    git shortlog -se 6c976bd..HEAD \
        | grep -v @apache.org \
        | sed $SED_ERE_FLAG -e "$CONTRIB_EMAIL_SED_COMMAND" >> $RELDIR/THANKS
    echo "" >> $RELDIR/THANKS # simplest portable newline
    echo "For a list of authors see the \`AUTHORS\` file." >> $RELDIR/THANKS
fi
