#!/bin/sh

# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    exec /usr/bin/ksh $0 "$@"
fi

# clear it so if we invoke other scripts, they run as ksh as well
unset POSIX_SHELL

## This script replaces the default "erl" in erts-VSN/bin. This is
## necessary as escript depends on erl and in turn, erl depends on
## having access to a bootscript (start.boot). Note that this script
## is ONLY invoked as a side-effect of running escript -- the embedded
## node bypasses erl and uses erlexec directly (as it should).
##
## Note that this script makes the assumption that there is a
## start_clean.boot file available in $ROOTDIR/release/VSN.

# Determine the abspath of where this script is executing from.
ERTS_BIN_DIR=$(cd ${0%/*} && pwd -P)

# Now determine the root directory -- this script runs from erts-VSN/bin,
# so we simply need to strip off two dirs from the end of the ERTS_BIN_DIR
# path.
ROOTDIR=${ERTS_BIN_DIR%/*/*}

# Parse out release and erts info
START_ERL=`cat $ROOTDIR/releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
APP_VSN=${START_ERL#* }

BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin
EMU=beam
PROGNAME=`echo $0 | sed 's/.*\\///'`
CMD="$BINDIR/erlexec"
export EMU
export ROOTDIR
export BINDIR
export PROGNAME

exec $CMD -boot $ROOTDIR/releases/$APP_VSN/start_clean ${1+"$@"}
