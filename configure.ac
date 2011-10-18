dnl Licensed under the Apache License, Version 2.0 (the "License"); you may not
dnl use this file except in compliance with the License.  dnl You may obtain a
dnl copy of the License at
dnl
dnl   http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl Unless required by applicable law or agreed to in writing, software
dnl distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
dnl WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
dnl License for the specific language governing permissions and limitations
dnl under the License.

AC_INIT([LOCAL_PACKAGE_NAME], [LOCAL_VERSION], [LOCAL_BUG_URI],
    [LOCAL_PACKAGE_TARNAME])

AC_PREREQ([2.59])

AC_CONFIG_SRCDIR([CHANGES])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([1.6.3 foreign])

AC_GNU_SOURCE
AC_ENABLE_SHARED
AC_DISABLE_STATIC

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_LN_S

AC_MSG_CHECKING([for pthread_create in -lpthread])

original_LIBS="$LIBS"
LIBS="-lpthread $original_LIBS"

AC_TRY_LINK([#include<pthread.h>],
    [pthread_create((void *)0, (void *)0, (void *)0, (void *)0)],
    [pthread=yes], [pthread=no])

if test x${pthread} = xyes; then
    AC_MSG_RESULT([yes])
else
    LIBS="$original_LIBS"
    AC_MSG_RESULT([no])
fi

AC_ARG_WITH([erlang], [AC_HELP_STRING([--with-erlang=PATH],
    [set PATH to the Erlang include directory])], [
    ERLANG_FLAGS="-I$withval"
], [
    ERLANG_FLAGS="-I${libdir}/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/lib/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/local/lib/erlang/usr/include"
    ERLANG_FLAGS="$ERLANG_FLAGS -I/opt/local/lib/erlang/usr/include"
])

AC_ARG_WITH([js-include], [AC_HELP_STRING([--with-js-include=PATH],
    [set PATH to the SpiderMonkey include directory])], [
    JS_INCLUDE="$withval"
    JS_FLAGS="-I$JS_INCLUDE"
], [
    JS_FLAGS="-I/usr/include"
    JS_FLAGS="$JS_FLAGS -I/usr/include/js"
    JS_FLAGS="$JS_FLAGS -I/usr/include/mozjs"
    JS_FLAGS="$JS_FLAGS -I/usr/local/include"
    JS_FLAGS="$JS_FLAGS -I/opt/local/include"
    JS_FLAGS="$JS_FLAGS -I/usr/local/include/js"
    JS_FLAGS="$JS_FLAGS -I/opt/local/include/js"
])

AC_ARG_WITH([js-lib], [AC_HELP_STRING([--with-js-lib=PATH],
    [set PATH to the SpiderMonkey library directory])],
    [
    JS_LIB_DIR=$withval
    JS_LIB_FLAGS="-L$withval"
], [
    JS_LIB_DIR=
])
AC_SUBST(JS_LIB_DIR)

use_js_trunk=no
AC_ARG_ENABLE([js-trunk], [AC_HELP_STRING([--enable-js-trunk],
    [allow use of SpiderMonkey versions newer than js185-1.0.0])], [
    use_js_trunk=$enableval
], [])

AC_ARG_VAR([ERLC_FLAGS], [general flags to prepend to ERLC_FLAGS])
AC_ARG_VAR([FLAGS], [general flags to prepend to LDFLAGS and CPPFLAGS])

LIB_FLAGS="$JS_LIB_FLAGS -L/usr/local/lib -L/opt/local/lib"
LIBS="$LIB_FLAGS $LIBS"

case "$(uname -s)" in
  CYGWIN*)
    FLAGS="$LIB_FLAGS $ERLANG_FLAGS $JS_FLAGS -DXP_WIN $FLAGS"
    CPPFLAGS="$FLAGS $CPPFLAGS"
    LDFLAGS="$FLAGS $LDFLAGS"
    IS_WINDOWS="TRUE"
    # The erlang cc.sh/ld.sh scripts will convert a -O option
    # into the same optimization flags erlang itself uses.
    CFLAGS="-O2"
    LTCFLAGS="$CFLAGS"
    ;;
  *)
    # XP_UNIX required for jsapi.h and has been tested to work on Linux and Darwin.
    FLAGS="$LIB_FLAGS $ERLANG_FLAGS $JS_FLAGS -DXP_UNIX $FLAGS"
    CPPFLAGS="$FLAGS $CPPFLAGS"
    # manually linking libm is requred for FreeBSD 7.0
    LDFLAGS="$FLAGS -lm $LDFLAGS"
    ;;
esac

AM_CONDITIONAL([WINDOWS], [test x$IS_WINDOWS = xTRUE])

AC_CHECK_LIB([mozjs185], [JS_NewContext], [JS_LIB_BASE=mozjs185], [
    AC_CHECK_LIB([mozjs185-1.0], [JS_NewContext], [JS_LIB_BASE=mozjs185-1.0], [
        AC_CHECK_LIB([mozjs], [JS_NewContext], [JS_LIB_BASE=mozjs], [
            AC_CHECK_LIB([js], [JS_NewContext], [JS_LIB_BASE=js], [
                AC_CHECK_LIB([js3250], [JS_NewContext], [JS_LIB_BASE=js3250], [
                    AC_CHECK_LIB([js32], [JS_NewContext], [JS_LIB_BASE=js32], [
                        AC_MSG_ERROR([Could not find the js library.

Is the Mozilla SpiderMonkey library installed?])])])])])])])

AC_SUBST(JS_LIB_BASE)

if test x${IS_WINDOWS} = xTRUE; then
    if test -f "$JS_LIB_DIR/$JS_LIB_BASE.dll"; then
        # seamonkey 1.7- build layout on Windows
        JS_LIB_BINARY="$JS_LIB_DIR/$JS_LIB_BASE.dll"
    else
        # seamonkey 1.8+ build layout on Windows
        if test -f "$JS_LIB_DIR/../bin/$JS_LIB_BASE.dll"; then
            JS_LIB_BINARY="$JS_LIB_DIR/../bin/$JS_LIB_BASE.dll"
        else
        AC_MSG_ERROR([Could not find $JS_LIB_BASE.dll.])
        fi
    fi
    AC_SUBST(JS_LIB_BINARY)

    # On windows we need to know the path to the openssl binaries.
    AC_ARG_WITH([openssl-bin-dir], [AC_HELP_STRING([--with-openssl-bin-dir=PATH],
        [path to the open ssl binaries for distribution on Windows])], [
        openssl_bin_dir=`cygpath -m "$withval"`
        AC_SUBST(openssl_bin_dir)
    ], [])

    # Windows uses Inno setup - look for its compiler.
    AC_PATH_PROG([INNO_COMPILER_EXECUTABLE], [iscc])
    if test x${INNO_COMPILER_EXECUTABLE} = x; then
        AC_MSG_WARN([You will be unable to build the Windows installer.])
    fi

    # We need the msvc redistributables for this platform too
    # (in theory we could just install the assembly locally - but
    # there are at least 4 directories with binaries, meaning 4 copies;
    # so using the redist .exe means it ends up installed globally...)
    AC_ARG_WITH([msvc-redist-dir], [AC_HELP_STRING([--with-msvc-redist-dir=PATH],
        [path to the msvc redistributables for the Windows platform])], [
        msvc_redist_dir=`cygpath -m "$withval"`
        msvc_redist_name="vcredist_x86.exe"
        AC_SUBST(msvc_redist_dir)
        AC_SUBST(msvc_redist_name)
    ], [])
    if test ! -f ${msvc_redist_dir}/${msvc_redist_name}; then
        AC_MSG_WARN([The MSVC redistributable seems to be missing; expect the installer to fail.])
    fi
fi

JSLIB=-l$JS_LIB_BASE

AC_CHECK_HEADER([jsapi.h], [], [
    AC_CHECK_HEADER([js/jsapi.h],
        [
        CPPFLAGS="$CPPFLAGS -I$JS_INCLUDE/js"
        ],
        [
            AC_MSG_ERROR([Could not find the jsapi header.

Are the Mozilla SpiderMonkey headers installed?])
        ])])

# Prevent people from accidentally using SpiderMonkey's that are too new

if test "$use_js_trunk" = "no"; then
AC_CHECK_DECL([JSOPTION_ANONFUNFIX], [], [
    AC_MSG_ERROR([Your SpiderMonkey library is too new.

Versions of SpiderMonkey after the js185-1.0.0 release remove the optional
enforcement of preventing anonymous functions in a statement context. This
will most likely break your existing JavaScript code as well as render all
example code invalid.

If you wish to ignore this error pass --enable-js-trunk to ./configure.])],
    [[#include <jsapi.h>]])
fi

AC_SUBST(JSLIB)

AC_LANG_PUSH(C)

AC_CHECK_LIB([$JS_LIB_BASE], [JS_NewCompartmentAndGlobalObject],
    AC_DEFINE([SM185], [1],
        [Use SpiderMonkey 1.8.5]))

AC_CHECK_LIB([$JS_LIB_BASE], [JS_ThrowStopIteration],
    AC_DEFINE([SM180], [1],
        [Use SpiderMonkey 1.8.0]))

AC_CHECK_LIB([$JS_LIB_BASE], [JS_GetStringCharsAndLength],
    AC_DEFINE([HAVE_JS_GET_STRING_CHARS_AND_LENGTH], [1],
        [Use newer JS_GetCharsAndLength function.]))

AC_LANG_POP(C)

AC_ARG_WITH([win32-icu-binaries], [AC_HELP_STRING([--with-win32-icu-binaries=PATH],
    [set PATH to the Win32 native ICU binaries directory])], [
    ICU_CONFIG="" # supposed to be a command to query options...
    ICU_LOCAL_CFLAGS="-I$withval/include"
    ICU_LOCAL_LDFLAGS="-L$withval/lib"
    ICU_LOCAL_BIN=$withval/bin
], [
    AC_CHECK_ICU([3.4.1])
    ICU_LOCAL_CFLAGS=`$ICU_CONFIG --cppflags-searchpath`
    ICU_LOCAL_LDFLAGS=`$ICU_CONFIG --ldflags-searchpath`
    ICU_LOCAL_BIN=
])

AC_SUBST(ICU_CONFIG)
AC_SUBST(ICU_LOCAL_CFLAGS)
AC_SUBST(ICU_LOCAL_LDFLAGS)
AC_SUBST(ICU_LOCAL_BIN)

AC_ARG_WITH([win32-curl], [AC_HELP_STRING([--with-win32-curl=PATH],
    [set PATH to the Win32 native curl directory])], [
    # default build on windows is a static lib, and that's what we want too
    CURL_CFLAGS="-I$withval/include -DCURL_STATICLIB"
    CURL_LIBS="$withval/lib/libcurl"
    CURL_LDFLAGS="-l$CURL_LIBS -lWs2_32 -lkernel32 -luser32 -ladvapi32 -lWldap32"
], [
    AC_CHECK_CURL([7.18.0])
    CURL_LDFLAGS=-lcurl
])

AC_SUBST(CURL_CFLAGS)
AC_SUBST(CURL_LIBS)
AC_SUBST(CURL_LDFLAGS)

case "$(uname -s)" in
  Linux)
    LIBS="$LIBS -lcrypt"
    CPPFLAGS="-D_XOPEN_SOURCE $CPPFLAGS"
    ;;
  FreeBSD)
    LIBS="$LIBS -lm -lcrypt"
    ;;
  OpenBSD)
    LIBS="$LIBS -lm -lcrypto"
  ;;
esac

AC_PATH_PROG([ERL], [erl])

if test x${ERL} = x; then
    AC_MSG_ERROR([Could not find the `erl' executable. Is Erlang installed?])
fi

erlang_version_error="The installed Erlang version is less than erts-5.7.3 (R13B02)."

version="`${ERL} -version 2>&1 | ${SED} "s/[[^0-9]]/ /g"`"

if test `echo $version | ${AWK} "{print \\$1}"` -lt 5; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -lt 7; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -eq 7; then
    if test `echo $version | ${AWK} "{print \\$3}"` -lt 3; then
        AC_MSG_ERROR([$erlang_version_error])
    fi
fi

has_crypto=`${ERL} -eval "case application:load(crypto) of ok -> ok; _ -> exit(no_crypto) end." -noshell -s init stop`

if test -n "$has_crypto"; then
    AC_MSG_ERROR([Could not find the Erlang crypto library. Has Erlang been compiled with OpenSSL support?])
fi

AC_PATH_PROG([ERLC], [erlc])

if test x${ERLC} = x; then
    AC_MSG_ERROR([Could not find the `erlc' executable. Is Erlang installed?])
fi

AC_CHECK_HEADER([erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the `erl_driver.h' header.

Are the Erlang headers installed? Use the `--with-erlang' option to specify the
path to the Erlang include directory.])])

AC_PATH_PROG([HELP2MAN_EXECUTABLE], [help2man])
if test x${HELP2MAN_EXECUTABLE} = x; then
    AC_MSG_WARN([You will be unable to regenerate any man pages.])
fi

use_init=yes
use_launchd=yes

AC_ARG_ENABLE([init], [AC_HELP_STRING([--disable-init],
    [don't install init script where applicable])], [
    use_init=$enableval
], [])

AC_ARG_ENABLE([launchd], [AC_HELP_STRING([--disable-launchd],
    [don't install launchd configuration where applicable])], [
    use_launchd=$enableval
], [])

init_enabled=false
launchd_enabled=false

if test "$use_init" = "yes"; then
   AC_MSG_CHECKING(location of init directory)
   if test -d /etc/rc.d; then
       init_enabled=true
       AC_SUBST([initdir], ['${sysconfdir}/rc.d'])
       AC_MSG_RESULT(${initdir})
   else
       if test -d /etc/init.d; then
           init_enabled=true
           AC_SUBST([initdir], ['${sysconfdir}/init.d'])
           AC_MSG_RESULT(${initdir})
       else
           AC_MSG_RESULT(not found)
       fi
    fi
fi

if test "$use_launchd" = "yes"; then
    AC_MSG_CHECKING(location of launchd directory)
    if test -d /Library/LaunchDaemons; then
        init_enabled=false
        launchd_enabled=true
        AC_SUBST([launchddir], ['${prefix}/Library/LaunchDaemons'])
        AC_MSG_RESULT(${launchddir})
    else
        AC_MSG_RESULT(not found)
    fi
fi

AC_ARG_VAR([ERL], [path to the `erl' executable])
AC_ARG_VAR([ERLC], [path to the `erlc' executable])
AC_ARG_VAR([HELP2MAN_EXECUTABLE], [path to the `help2man' program])

if test -n "$HELP2MAN_EXECUTABLE"; then
    help2man_enabled=true
else
    if test -f "$srcdir/bin/couchdb.1" -a -f "$srcdir/src/couchdb/priv/couchjs.1"; then
        help2man_enabled=true
    else
        help2man_enabled=false
    fi
fi

AM_CONDITIONAL([INIT], [test x${init_enabled} = xtrue])
AM_CONDITIONAL([LAUNCHD], [test x${launchd_enabled} = xtrue])
AM_CONDITIONAL([HELP2MAN], [test x${help2man_enabled} = xtrue])

AC_SUBST([package_author_name], ["LOCAL_PACKAGE_AUTHOR_NAME"])
AC_SUBST([package_author_address], ["LOCAL_PACKAGE_AUTHOR_ADDRESS"])
AC_SUBST([package_identifier], ["LOCAL_PACKAGE_IDENTIFIER"])
AC_SUBST([package_tarname], ["LOCAL_PACKAGE_TARNAME"])
AC_SUBST([package_name], ["LOCAL_PACKAGE_NAME"])

AC_SUBST([version], ["LOCAL_VERSION"])
AC_SUBST([version_major], ["LOCAL_VERSION_MAJOR"])
AC_SUBST([version_minor], ["LOCAL_VERSION_MINOR"])
AC_SUBST([version_revision], ["LOCAL_VERSION_REVISION"])
AC_SUBST([version_stage], ["LOCAL_VERSION_STAGE"])
AC_SUBST([version_release], ["LOCAL_VERSION_RELEASE"])

AC_SUBST([bug_uri], ["LOCAL_BUG_URI"])

AC_SUBST([localconfdir], [${sysconfdir}/${package_identifier}])
AC_SUBST([localdatadir], [${datadir}/${package_identifier}])
AC_SUBST([localdocdir], [${datadir}/doc/${package_identifier}])
AC_SUBST([locallibdir], [${libdir}/${package_identifier}])
AC_SUBST([localstatelibdir], [${localstatedir}/lib/${package_identifier}])
AC_SUBST([localstatelogdir], [${localstatedir}/log/${package_identifier}])
AC_SUBST([localstaterundir], [${localstatedir}/run/${package_identifier}])

# On Windows we install directly into our erlang distribution.
if test x${IS_WINDOWS} = xTRUE; then
    AC_SUBST([locallibbindir], [${prefix}/bin])
    AC_SUBST([localerlanglibdir], [${libdir}])
else
    AC_SUBST([locallibbindir], [${locallibdir}/bin])
    AC_SUBST([localerlanglibdir], [${locallibdir}/erlang/lib])
fi

# fix for older autotools that don't define "abs_top_YYY" by default
AC_SUBST(abs_top_srcdir)
AC_SUBST(abs_top_builddir)

AC_REVISION([LOCAL_VERSION])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([bin/couchdb.tpl])
AC_CONFIG_FILES([bin/couchdb.bat.tpl])
AC_CONFIG_FILES([bin/Makefile])
AC_CONFIG_FILES([etc/couchdb/Makefile])
AC_CONFIG_FILES([etc/couchdb/default.ini.tpl])
AC_CONFIG_FILES([etc/default/Makefile])
AC_CONFIG_FILES([etc/init/couchdb.tpl])
AC_CONFIG_FILES([etc/init/Makefile])
AC_CONFIG_FILES([etc/launchd/org.apache.couchdb.plist.tpl])
AC_CONFIG_FILES([etc/launchd/Makefile])
AC_CONFIG_FILES([etc/logrotate.d/couchdb.tpl])
AC_CONFIG_FILES([etc/logrotate.d/Makefile])
AC_CONFIG_FILES([etc/windows/Makefile])
AC_CONFIG_FILES([etc/Makefile])
AC_CONFIG_FILES([share/Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/couchdb/couch.app.tpl])
AC_CONFIG_FILES([src/couchdb/Makefile])
AC_CONFIG_FILES([src/couchdb/priv/Makefile])
AC_CONFIG_FILES([src/erlang-oauth/Makefile])
AC_CONFIG_FILES([src/etap/Makefile])
AC_CONFIG_FILES([src/ibrowse/Makefile])
AC_CONFIG_FILES([src/mochiweb/Makefile])
AC_CONFIG_FILES([test/Makefile])
AC_CONFIG_FILES([test/bench/Makefile])
AC_CONFIG_FILES([test/etap/Makefile])
AC_CONFIG_FILES([test/etap/test_util.erl])
AC_CONFIG_FILES([test/javascript/Makefile])
AC_CONFIG_FILES([test/view_server/Makefile])
AC_CONFIG_FILES([utils/Makefile])
AC_CONFIG_FILES([var/Makefile])

AC_OUTPUT

# Windows Erlang build tools wrap Microsoft's linker and compiler just enough
# to be able to build Erlang/OTP successfully, but not enough for full
# compatibility with GNU AutoTools. The MS VC compiler and linker are
# hidden from autotools in Erlang's cc.sh and ld.sh wrappers. GNU autoconf
# identifies this dastardly mix as a unix variant, and libtool kindly
# passes incorrect flags and names through to the MS linker. The simplest fix
# is to modify libtool via sed to remove those options.
# As this is only done once at first configure, and subsequent config or source
# changes may trigger a silent reversion to the non-functioning original.
# Changes are;
# 1. replace LIB$name with $name in libname_spec (e.g. libicu -> icu) to ensure
#    correct windows versions of .lib and .dlls are found or generated.
# 2. remove incompatible \w-link\w from archive_cmds
# 3. remove GNU-style directives to be passed through to the linker
# 4. swap GNU-style shared library flags with MS -dll variant
# This obscene hackery is tracked under COUCHDB-440 and COUCHDB-1197.

if test x${IS_WINDOWS} = xTRUE; then
    mv libtool libtool.dist
    /bin/sed -E -e 's,^libname_spec="lib,libname_spec=",' \
        -e 's,( -link ), ,' \
        -e 's,-Xlinker --out-implib -Xlinker \\\$lib,,' \
        -e 's,(-shared -nostdlib), -dll ,' \
        < libtool.dist > libtool
    # probably would chmod +x if we weren't on windows...
fi

echo
echo "You have configured Apache CouchDB, time to relax."
echo
echo "Run \`make && sudo make install' to install."
