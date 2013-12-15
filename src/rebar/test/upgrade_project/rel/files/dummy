#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

# /bin/sh on Solaris is not a POSIX compatible shell, but /usr/bin/ksh is.
if [ `uname -s` = 'SunOS' -a "${POSIX_SHELL}" != "true" ]; then
    POSIX_SHELL="true"
    export POSIX_SHELL
    # To support 'whoami' add /usr/ucb to path
    PATH=/usr/ucb:$PATH
    export PATH
    exec /usr/bin/ksh $0 "$@"
fi

# clear it so if we invoke other scripts, they run as ksh
unset POSIX_SHELL

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd -P)

CALLER_DIR=$PWD

RUNNER_BASE_DIR=${RUNNER_SCRIPT_DIR%/*}
RUNNER_ETC_DIR=$RUNNER_BASE_DIR/etc
# Note the trailing slash on $PIPE_DIR/
PIPE_DIR=/tmp/$RUNNER_BASE_DIR/
RUNNER_USER=
WHOAMI=$(whoami)

# Make sure this script is running as the appropriate user
if ([ "$RUNNER_USER" ] && [ "x$WHOAMI" != "x$RUNNER_USER" ]); then
    type sudo > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "sudo doesn't appear to be installed and your EUID isn't $RUNNER_USER" 1>&2
        exit 1
    fi
    echo "Attempting to restart script through sudo -H -u $RUNNER_USER" >&2
    exec sudo -H -u $RUNNER_USER -i $RUNNER_SCRIPT_DIR/$RUNNER_SCRIPT $@
fi

# Identify the script name
SCRIPT=`basename $0`

# Parse out release and erts info
START_ERL=`cat $RUNNER_BASE_DIR/releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
APP_VSN=${START_ERL#* }

# Use $CWD/vm.args if exists, otherwise releases/APP_VSN/vm.args, or
# else etc/vm.args
if [ -e "$CALLER_DIR/vm.args" ]; then
    VMARGS_PATH=$CALLER_DIR/vm.args
    USE_DIR=$CALLER_DIR
else
    USE_DIR=$RUNNER_BASE_DIR
    if [ -e "$RUNNER_BASE_DIR/releases/$APP_VSN/vm.args" ]; then
        VMARGS_PATH="$RUNNER_BASE_DIR/releases/$APP_VSN/vm.args"
    else
        VMARGS_PATH="$RUNNER_ETC_DIR/vm.args"
    fi
fi

RUNNER_LOG_DIR=$USE_DIR/log
# Make sure log directory exists
mkdir -p $RUNNER_LOG_DIR

# Use releases/VSN/sys.config if it exists otherwise use etc/app.config
if [ -e "$USE_DIR/sys.config" ]; then
    CONFIG_PATH="$USE_DIR/sys.config"
else
    if [ -e "$RUNNER_BASE_DIR/releases/$APP_VSN/sys.config" ]; then
        CONFIG_PATH="$RUNNER_BASE_DIR/releases/$APP_VSN/sys.config"
    else
        CONFIG_PATH="$RUNNER_ETC_DIR/app.config"
    fi
fi

# Extract the target node name from node.args
NAME_ARG=`egrep '^\-s?name' $VMARGS_PATH`
if [ -z "$NAME_ARG" ]; then
    echo "vm.args needs to have either -name or -sname parameter."
    exit 1
fi

# Extract the name type and name from the NAME_ARG for REMSH
REMSH_TYPE=`echo $NAME_ARG | awk '{print $1}'`
REMSH_NAME=`echo $NAME_ARG | awk '{print $2}'`

# Note the `date +%s`, used to allow multiple remsh to the same node
# transparently
REMSH_NAME_ARG="$REMSH_TYPE remsh`date +%s`@`echo $REMSH_NAME | awk -F@ '{print $2}'`"
REMSH_REMSH_ARG="-remsh $REMSH_NAME"

# Extract the target cookie
COOKIE_ARG=`grep '^\-setcookie' $VMARGS_PATH`
if [ -z "$COOKIE_ARG" ]; then
    echo "vm.args needs to have a -setcookie parameter."
    exit 1
fi

# Make sure CWD is set to the right dir
cd $USE_DIR

# Make sure log directory exists
mkdir -p $USE_DIR/log

# Add ERTS bin dir to our path
ERTS_PATH=$RUNNER_BASE_DIR/erts-$ERTS_VSN/bin

# Setup command to control the node
NODETOOL="$ERTS_PATH/escript $ERTS_PATH/nodetool $NAME_ARG $COOKIE_ARG"

# Setup remote shell command to control node
REMSH="$ERTS_PATH/erl $REMSH_NAME_ARG $REMSH_REMSH_ARG $COOKIE_ARG"

# Common functions

# Ping node without allowing nodetool to take stdin
ping_node() {
    $NODETOOL ping < /dev/null
}

# Set the PID global variable, return 1 on error
get_pid() {
    PID=`$NODETOOL getpid < /dev/null`
    ES=$?
    if [ "$ES" -ne 0 ]; then
        echo "Node is not running!"
        return 1
    fi

    # don't allow empty or init pid's
    if [ -z $PID ] || [ "$PID" -le 1 ]; then
        return 1
    fi

    return 0
}

# Check the first argument for instructions
case "$1" in
    start|start_boot)
        # Make sure there is not already a node running
        RES=`ping_node`
        if [ "$RES" = "pong" ]; then
            echo "Node is already running!"
            exit 1
        fi
        case "$1" in
            start)
                shift
                START_OPTION="console"
                HEART_OPTION="start"
                ;;
            start_boot)
                shift
                START_OPTION="console_boot"
                HEART_OPTION="start_boot"
                ;;
        esac
        RUN_PARAM=$(printf "\'%s\' " "$@")
        HEART_COMMAND="$RUNNER_BASE_DIR/bin/$SCRIPT $HEART_OPTION $RUN_PARAM"
        export HEART_COMMAND
        mkdir -p $PIPE_DIR
        $ERTS_PATH/run_erl -daemon $PIPE_DIR $RUNNER_LOG_DIR "exec $RUNNER_BASE_DIR/bin/$SCRIPT $START_OPTION $RUN_PARAM" 2>&1
        ;;

    stop)
        # Wait for the node to completely stop...
        case `uname -s` in
            Darwin)
                # Make sure we explicitly set this because iTerm.app doesn't for
                # some reason.
                COMMAND_MODE=unix2003
        esac

        # Get the PID from nodetool
        get_pid
        GPR=$?
        if [ "$GPR" -ne 0 ] || [ -z $PID ]; then
            exit $GPR
        fi

        # Tell nodetool to initiate a stop
        $NODETOOL stop
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi

        # Wait for the node to completely stop...
        while `kill -s 0 $PID 2>/dev/null`
        do
            sleep 1
        done
        ;;

    restart)
        ## Restart the VM without exiting the process
        $NODETOOL restart
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    reboot)
        ## Restart the VM completely (uses heart to restart it)
        $NODETOOL reboot
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    ping)
        ## See if the VM is alive
        ping_node
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    attach)
        # Make sure a node is running
        ping_node
        ES=$?
        if [ "$ES" -ne 0 ]; then
            echo "Node is not running!"
            exit $ES
        fi

        shift
        exec $ERTS_PATH/to_erl $PIPE_DIR
        ;;

    remote_console)
        # Make sure a node is running
        ping_node
        ES=$?
        if [ "$ES" -ne 0 ]; then
            echo "Node is not running!"
            exit $ES
        fi

        shift
        exec $REMSH
        ;;

    upgrade)
        if [ -z "$2" ]; then
            echo "Missing upgrade package argument"
            echo "Usage: $SCRIPT upgrade {package base name}"
            echo "NOTE {package base name} MUST NOT include the .tar.gz suffix"
            exit 1
        fi

        # Make sure a node IS running
        ping_node
        ES=$?
        if [ "$ES" -ne 0 ]; then
            echo "Node is not running!"
            exit $ES
        fi

        node_name=`echo $NAME_ARG | awk '{print $2}'`
        erlang_cookie=`echo $COOKIE_ARG | awk '{print $2}'`

        $ERTS_PATH/escript $RUNNER_BASE_DIR/bin/install_upgrade.escript $node_name $erlang_cookie $2
        ;;

    console|console_clean|console_boot)
        # .boot file typically just $SCRIPT (ie, the app name)
        # however, for debugging, sometimes start_clean.boot is useful.
        # For e.g. 'setup', one may even want to name another boot script.
        case "$1" in
            console)        BOOTFILE=$SCRIPT ;;
            console_clean)  BOOTFILE=start_clean ;;
            console_boot)
                shift
                BOOTFILE="$1"
                shift
                ;;
        esac
        # Setup beam-required vars
        ROOTDIR=$RUNNER_BASE_DIR
        BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin
        EMU=beam
        PROGNAME=`echo $0 | sed 's/.*\\///'`
        CMD="$BINDIR/erlexec -boot $RUNNER_BASE_DIR/releases/$APP_VSN/$BOOTFILE -mode embedded -config $CONFIG_PATH -args_file $VMARGS_PATH"
        export EMU
        export ROOTDIR
        export BINDIR
        export PROGNAME

        # Dump environment info for logging purposes
        echo "Exec: $CMD" -- ${1+"$@"}
        echo "Root: $ROOTDIR"

        # Log the startup
        logger -t "$SCRIPT[$$]" "Starting up"

        # Start the VM
        exec $CMD -- ${1+"$@"}
        ;;

    foreground)
        # start up the release in the foreground for use by runit
        # or other supervision services

        BOOTFILE=$SCRIPT
        FOREGROUNDOPTIONS="-noinput +Bd"

        # Setup beam-required vars
        ROOTDIR=$RUNNER_BASE_DIR
        BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin
        EMU=beam
        PROGNAME=`echo $0 | sed 's/.*\///'`
        CMD="$BINDIR/erlexec $FOREGROUNDOPTIONS -boot $RUNNER_BASE_DIR/releases/$APP_VSN/$BOOTFILE -config $CONFIG_PATH -args_file $VMARGS_PATH"
        export EMU
        export ROOTDIR
        export BINDIR
        export PROGNAME

        # Dump environment info for logging purposes
        echo "Exec: $CMD" -- ${1+"$@"}
        echo "Root: $ROOTDIR"

        # Start the VM
        exec $CMD -- ${1+"$@"}
        ;;
    getpid)
        # Get the PID from nodetool
        get_pid
        ES=$?
        if [ "$ES" -ne 0 ] || [ -z $PID ]; then
            exit $ES
        fi
        echo $PID
        ;;
    *)
        echo "Usage: $SCRIPT {start|start_boot <file>|foreground|stop|restart|reboot|ping|console|getpid|console_clean|console_boot <file>|attach|remote_console|upgrade}"
        exit 1
        ;;
esac

exit 0
