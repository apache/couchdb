#!/bin/sh -e

VERSION=$1

if [ -z "$VERSION" ]; then
  echo "NO VERSION"
  exit 1
fi

echo "Building Apache CouchDB $VERSION"

REL_DIR=apache-couchdb-$VERSION
# make release dir
rm -rf $REL_DIR
mkdir $REL_DIR

# copy sources over
git archive HEAD | tar -xC $REL_DIR/ -f -
cd src/

for repo in *; do
  cd $repo
  if [ -d ".git" ]; then
    mkdir -p ../../$REL_DIR/src/$repo
    git archive HEAD | tar --exclude '*do_not_compile.erl' -xC ../../$REL_DIR/src/$repo/ -f -
  fi
  cd ..
done

cd ..


if test -e .git; then
    # save git sha in version.mk
    git_sha=`git rev-parse --short=7 HEAD`
    echo "git_sha=$git_sha" >> $REL_DIR/version.mk
    # create CONTRIBUTORS file
    OS=`uname -s`
    case "$OS" in
    Linux|CYGWIN*) # GNU sed
        SED_ERE_FLAG=-r
    ;;
    *) # BSD sed
        SED_ERE_FLAG=-E
    ;;
    esac

    sed -e "/^#.*/d" CONTRIBUTORS.in > $REL_DIR/CONTRIBUTORS
    CONTRIB_EMAIL_SED_COMMAND="s/^[[:blank:]]{5}[[:digit:]]+[[:blank:]]/ * /"
    git shortlog -se 6c976bd..HEAD \
        | grep -v @apache.org \
        | sed $SED_ERE_FLAG -e "$CONTRIB_EMAIL_SED_COMMAND" >> $REL_DIR/CONTRIBUTORS
    echo "" >> $REL_DIR/CONTRIBUTORS # simplest portable newline
    echo "For a list of authors see the \`AUTHORS\` file." >> $REL_DIR/CONTRIBUTORS
fi

# copy our rebar
cp bin/rebar $REL_DIR/bin/rebar
