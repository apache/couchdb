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

AC_CONFIG_HEADER([config.h])
AC_CONFIG_HEADERS([src/snappy/google-snappy/config.h])

AM_INIT_AUTOMAKE([1.6.3 foreign])

AC_GNU_SOURCE
AC_ENABLE_SHARED
AC_DISABLE_STATIC

AC_PROG_CC
LT_INIT([win32-dll])
AC_PROG_LIBTOOL
AC_PROG_LN_S

PKG_PROG_PKG_CONFIG

dnl Config for google snappy
m4_define([snappy_major], [1])
m4_define([snappy_minor], [0])
m4_define([snappy_patchlevel], [3])

AC_PROG_CXX
AC_LANG([C++])
AC_C_BIGENDIAN
AC_CHECK_HEADERS([stdint.h stddef.h sys/mman.h sys/resource.h])
AC_CHECK_FUNC([mmap])

AC_MSG_CHECKING([if the compiler supports __builtin_expect])

AC_TRY_COMPILE(, [
    return __builtin_expect(1, 1) ? 1 : 0
], [
    snappy_have_builtin_expect=yes
    AC_MSG_RESULT([yes])
], [
    snappy_have_builtin_expect=no
    AC_MSG_RESULT([no])
])
if test x$snappy_have_builtin_expect = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_EXPECT], [1], [Define to 1 if the compiler supports __builtin_expect.])
fi

AC_MSG_CHECKING([if the compiler supports __builtin_ctzll])

AC_TRY_COMPILE(, [
    return (__builtin_ctzll(0x100000000LL) == 32) ? 1 : 0
], [
    snappy_have_builtin_ctz=yes
    AC_MSG_RESULT([yes])
], [
    snappy_have_builtin_ctz=no
    AC_MSG_RESULT([no])
])
if test x$snappy_have_builtin_ctz = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_CTZ], [1], [Define to 1 if the compiler supports __builtin_ctz and friends.])
fi

if test "$ac_cv_header_stdint_h" = "yes"; then
    AC_SUBST([ac_cv_have_stdint_h], [1])
else
    AC_SUBST([ac_cv_have_stdint_h], [0])
fi
if test "$ac_cv_header_stddef_h" = "yes"; then
    AC_SUBST([ac_cv_have_stddef_h], [1])
else
    AC_SUBST([ac_cv_have_stddef_h], [0])
fi

SNAPPY_MAJOR="snappy_major"
SNAPPY_MINOR="snappy_minor"
SNAPPY_PATCHLEVEL="snappy_patchlevel"

AC_SUBST([SNAPPY_MAJOR])
AC_SUBST([SNAPPY_MINOR])
AC_SUBST([SNAPPY_PATCHLEVEL])
dnl End of google snappy specific config

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

AC_ARG_VAR([ERLC_FLAGS], [general flags to prepend to ERLC_FLAGS])
AC_ARG_VAR([FLAGS], [general flags to prepend to LDFLAGS and CPPFLAGS])
AS_CASE([$(uname -s)],
    [CYGWIN*], [] ,
    [*], [
    CPPFLAGS="$CPPFLAGS -I/opt/local/include"
    CPPFLAGS="$CPPFLAGS -I/usr/local/include"
    CPPFLAGS="$CPPFLAGS -I/usr/include"
    LDFLAGS="$LDFLAGS -L/opt/local/lib"
    LDFLAGS="$LDFLAGS -L/usr/local/lib"
])
CPPFLAGS="$CPPFLAGS $FLAGS"
LDFLAGS="$LDFLAGS $FLAGS"

AC_PATH_PROG([ERL], [erl])
AS_IF([test x${ERL} = x], [
    AC_MSG_ERROR([Could not find the `erl' executable. Is Erlang installed?])
    ])

AC_ARG_WITH([erlang], [AC_HELP_STRING([--with-erlang=PATH],
    [set PATH to the Erlang include directory])], [
    ERLANG_FLAGS="-I$withval"
], [
    realerl=`readlink -f $ERL 2>/dev/null`
    AS_IF([test $? -eq 0], [
           erlbase=`dirname $realerl`
           erlbase=`dirname $erlbase`
           ERLANG_FLAGS="-I${erlbase}/usr/include"
          ], [
           # Failed to figure out where erl is installed..
           # try to add some default directories to search
           ERLANG_FLAGS="-I${libdir}/erlang/usr/include"
           ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/lib/erlang/usr/include"
           ERLANG_FLAGS="$ERLANG_FLAGS -I/usr/local/lib/erlang/usr/include"
           ERLANG_FLAGS="$ERLANG_FLAGS -I/opt/local/lib/erlang/usr/include"
          ])
])
AC_SUBST(ERLANG_FLAGS)

PKG_CHECK_EXISTS([mozjs185], [
    PKG_CHECK_EXISTS([nspr], [], [
        AC_MSG_WARN([

You have the pkg-config file for mozjs185 isntalled but no pkg-config
file for NSPR. More than likely configure will fail. If it does it most
likely means you need to find on install the pkg-config file for NSPR.
This most commonly occurs on Mac OS X with older versions of Homebrew.

You can correct this by removing SpiderMonkey and NSPR, updating
Homebrew and reinstalling.

])])])

AC_ARG_WITH([js-lib], [AC_HELP_STRING([--with-js-lib=PATH],
            [set PATH to the SpiderMonkey library directory])],
    [
    JS_LIB_DIR=$withval
], [
    PKG_CHECK_MODULES([JS185], [mozjs185], [
    JS_LIB_DIR="$(${PKG_CONFIG} --variable=libdir mozjs185)"
    ], [
        PKG_CHECK_MODULES([JS], [mozilla-js >= 1.7], [
            JS_LIB_DIR="$(${PKG_CONFIG} --variable=sdkdir mozilla-js)/lib"
        ], [
            JS_LIB_DIR="${libdir}"
        ])
    ])
])
JS_LDFLAGS="-L$JS_LIB_DIR $LDFLAGS"


AC_ARG_WITH([js-include], [AC_HELP_STRING([--with-js-include=PATH],
    [set PATH to the SpiderMonkey include directory])], [
    JS_INCLUDE="$withval"
    JS_CFLAGS="-I$JS_INCLUDE"
    JS_CFLAGS="$JS_CFLAGS -I$JS_INCLUDE/js"
    JS_CFLAGS="$JS_CFLAGS -I$JS_INCLUDE/mozjs"
], [
    PKG_CHECK_MODULES([JS185], [mozjs185], [
        JS_CFLAGS="$(${PKG_CONFIG} --cflags mozjs185)"
    ], [
        PKG_CHECK_MODULES([JS], [mozilla-js >= 1.7], [
            JS_CFLAGS="$(${PKG_CONFIG} --cflags mozilla-js)"
        ], [
            JS_CFLAGS="-I/usr/include"
            JS_CFLAGS="$JS_CFLAGS -I/usr/include/js"
            JS_CFLAGS="$JS_CFLAGS -I/usr/include/mozjs"
            JS_CFLAGS="$JS_CFLAGS -I/usr/local/include/js"
            JS_CFLAGS="$JS_CFLAGS -I/opt/local/include/js"
        ])
    ])
])

use_js_trunk=no
AC_ARG_ENABLE([js-trunk], [AC_HELP_STRING([--enable-js-trunk],
    [allow use of SpiderMonkey versions newer than js185-1.0.0])], [
    use_js_trunk=$enableval
], [])

# The erlang cc.sh/ld.sh scripts will convert a -O option
# into the same optimization flags erlang itself uses.
CFLAGS="-O2 $CFLAGS"

LIBS="$LIBS $LDFLAGS"

AS_CASE([$(uname -s)],
    [CYGWIN*], [
        JS_CFLAGS="-DXP_WIN $JS_CFLAGS"
        IS_WINDOWS="TRUE"
    ] ,
    [*], [
        # XP_UNIX required for jsapi.h and has been tested to work on Linux and Darwin.
        JS_CFLAGS="-DXP_UNIX $JS_CFLAGS"
])

AM_CONDITIONAL([WINDOWS], [test x$IS_WINDOWS = xTRUE])

OLD_CPPFLAGS="$CPPFLAGS"
OLD_LDFLAGS="$LDFLAGS"
OLD_LIBS="$LIBS"
CPPFLAGS="$JS_CFLAGS $CPPFLAGS"
LDFLAGS="$JS_LDFLAGS"
LIBS="$JS_LIBS $LIBS"

AC_CHECK_HEADER([jsapi.h], [], [
    AC_CHECK_HEADER([js/jsapi.h],
        [
        CPPFLAGS="$CPPFLAGS -I$JS_INCLUDE/js"
        ],
        [
            AC_MSG_ERROR([Could not find the jsapi header.

Are the Mozilla SpiderMonkey headers installed?])
        ])])

AC_ARG_WITH([js-lib-name], [AC_HELP_STRING([--with-js-lib-name=NAME],
    [set Spidermonkey library NAME])], [
    JS_LIB_BASE="$withval"
    AC_CHECK_LIB([$JS_LIB_BASE], JS_NewObject, [], [
        AC_MSG_ERROR([Could not find the Spidermonkey library.

Did you specify the correct library name?])])
    ], [
    AC_CHECK_LIB(mozjs, [JS_NewObject], [JS_LIB_BASE=mozjs], [
        AC_CHECK_LIB(js, [JS_NewObject], [JS_LIB_BASE=js], [
            AC_CHECK_LIB([js3250], [JS_NewObject], [JS_LIB_BASE=js3250], [
                AC_CHECK_LIB([js32], [JS_NewObject], [JS_LIB_BASE=js32], [
                    AC_CHECK_LIB([mozjs185-1.0], [JS_NewObject], [JS_LIB_BASE=mozjs185-1.0], [
                        AC_CHECK_LIB(mozjs185, [JS_NewObject], [JS_LIB_BASE=mozjs185], [
                            AC_MSG_ERROR([Could not find the js library.

Is the Mozilla SpiderMonkey library installed?])])])])])])])
])

# Figure out what version of SpiderMonkey to use

AC_CHECK_LIB([$JS_LIB_BASE], [JS_NewCompartmentAndGlobalObject],
    # Prevent people from accidentally using SpiderMonkey's that are too new

    if test "$use_js_trunk" = "no"; then
        AC_CHECK_DECL([JSOPTION_ANONFUNFIX], [], [
            AC_MSG_ERROR([Your SpiderMonkey library is too new.

NOTE: Check above for an error about NSPR

Versions of SpiderMonkey after the js185-1.0.0 release remove the optional
enforcement of preventing anonymous functions in a statement context. This
will most likely break your existing JavaScript code as well as render all
example code invalid.

If you wish to ignore this error pass --enable-js-trunk to ./configure.])],
        [[#include <jsapi.h>]])
    fi
    AC_DEFINE([SM185], [1],
        [Use SpiderMonkey 1.8.5]))

AC_CHECK_LIB([$JS_LIB_BASE], [JS_ThrowStopIteration],
    AC_DEFINE([SM180], [1],
        [Use SpiderMonkey 1.8.0]))

AC_CHECK_LIB([$JS_LIB_BASE], [JS_GetStringCharsAndLength],
    AC_DEFINE([HAVE_JS_GET_STRING_CHARS_AND_LENGTH], [1],
        [Use newer JS_GetCharsAndLength function.]))

# Else, hope that 1.7.0 works

# Deal with JSScript -> JSObject -> JSScript switcheroo

AC_CHECK_TYPE([JSScript*],
    [AC_DEFINE([JSSCRIPT_TYPE], [JSScript*], [Use JSObject* for scripts])],
    [AC_DEFINE([JSSCRIPT_TYPE], [JSObject*], [Use JSScript* for scripts])],
    [[#include <jsapi.h>]]
)

AC_DEFINE([COUCHJS_NAME], ["couchjs"], ["CouchJS executable name."])

if test x${IS_WINDOWS} = xTRUE; then

    AC_DEFINE([COUCHJS_NAME], ["couchjs.exe"], ["CouchJS executable name."])

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

JS_CFLAGS="$CPPFLAGS"
JS_LDFLAGS="$LDFLAGS"
JS_LIBS="-l$JS_LIB_BASE -lm $LIBS"
CPPFLAGS="$OLD_CPPFLAGS"
LDFLAGS="$OLD_LDFLAGS"
LIBS="$OLD_LIBS"

AC_SUBST(JS_CFLAGS)
AC_SUBST(JS_LDFLAGS)
AC_SUBST(JS_LIBS)

AC_ARG_WITH([win32-icu-binaries], [AC_HELP_STRING([--with-win32-icu-binaries=PATH],
    [set PATH to the Win32 native ICU binaries directory])], [
    ICU_CPPFLAGS="-I$withval/include"
    ICU_LIBS="-L$withval/lib -licuuc -licudt -licuin"
    ICU_BIN=$withval/bin
], [
    AC_CHECK_ICU([3.4.1])
    ICU_BIN=
])

AC_SUBST(ICU_CFLAGS)
AC_SUBST(ICU_CPPFLAGS)
AC_SUBST(ICU_LIBS)
AC_SUBST(ICU_BIN)

use_curl=yes
AC_ARG_WITH([win32-curl], [AC_HELP_STRING([--with-win32-curl=PATH],
    [set PATH to the Win32 native curl directory])], [
    # default build on windows is a static lib, and that's what we want too
    CURL_CFLAGS="-I$withval/include -DCURL_STATICLIB"
    CURL_LIBS="-L$withval/lib -llibcurl -lWs2_32 -lkernel32 -luser32 -ladvapi32 -lWldap32"
    # OpenSSL libraries may be pulled in via libcurl if it was built with SSL
    # these are libeay32 ssleay32 instead of crypto ssl on unix
], [
    AC_CHECK_CURL([7.18.0],
        [AC_DEFINE([HAVE_CURL], [1], ["Provide HTTP support to couchjs"])], [
        AC_MSG_WARN([You will be unable to run some JavaScript unit tests.])
        use_curl=no
        CURL_LIBS=
    ])
])

AC_SUBST(CURL_CFLAGS)
AC_SUBST(CURL_LIBS)

case "$(uname -s)" in
  Linux)
    LIBS="$LIBS -lcrypt"
    CPPFLAGS="-D_XOPEN_SOURCE $CPPFLAGS"
    ;;
  FreeBSD)
    LIBS="$LIBS -lcrypt"
    ;;
  OpenBSD)
    LIBS="$LIBS -lcrypto"
  ;;
esac


erlang_version_error="The installed Erlang version is less than 5.6.5 (R12B05)."

version="`${ERL} -version 2>&1 | ${SED} 's/[[^0-9]]/ /g'`"

if test `echo $version | ${AWK} "{print \\$1}"` -lt 5; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -lt 6; then
    AC_MSG_ERROR([$erlang_version_error])
fi

if test `echo $version | ${AWK} "{print \\$2}"` -eq 6; then
    if test `echo $version | ${AWK} "{print \\$3}"` -lt 5; then
        AC_MSG_ERROR([$erlang_version_error])
    fi
fi

otp_release="`${ERL} -noshell -eval 'io:put_chars(erlang:system_info(otp_release)).' -s erlang halt`"
AC_SUBST(otp_release)
AM_CONDITIONAL([USE_OTP_NIFS], [test x$otp_release \> xR13B03])
AM_CONDITIONAL([USE_EJSON_COMPARE_NIF], [test x$otp_release \> xR14B03])

has_crypto=`${ERL} -eval "case application:load(crypto) of ok -> ok; _ -> exit(no_crypto) end." -noshell -s init stop`

if test -n "$has_crypto"; then
    AC_MSG_ERROR([Could not find the Erlang crypto library. Has Erlang been compiled with OpenSSL support?])
fi

AC_PATH_PROG([ERLC], [erlc])

if test x${ERLC} = x; then
    AC_MSG_ERROR([Could not find the `erlc' executable. Is Erlang installed?])
fi

OLD_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$ERLANG_FLAGS $CPPFLAGS"
AC_CHECK_HEADER([erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the `erl_driver.h' header.

Are the Erlang headers installed? Use the `--with-erlang' option to specify the
path to the Erlang include directory.])])
CPPFLAGS="$OLD_CPPFLAGS"

AC_PATH_PROG([HELP2MAN_EXECUTABLE], [help2man])
if test x${HELP2MAN_EXECUTABLE} = x; then
    AC_MSG_WARN([You will be unable to regenerate any man pages.])
fi

use_init=yes
use_launchd=yes
native_mochijson_enabled=no

AC_ARG_ENABLE([init], [AC_HELP_STRING([--disable-init],
    [don't install init script where applicable])], [
    use_init=$enableval
], [])

AC_ARG_ENABLE([launchd], [AC_HELP_STRING([--disable-launchd],
    [don't install launchd configuration where applicable])], [
    use_launchd=$enableval
], [])

AC_ARG_ENABLE([native-mochijson], [AC_HELP_STRING([--enable-native-mochijson],
    [compile mochijson to native code (EXPERIMENTAL)])], [
    native_mochijson_enabled=$enableval
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
AM_CONDITIONAL([USE_NATIVE_MOCHIJSON], [test x${native_mochijson_enabled} = xyes])
AM_CONDITIONAL([USE_CURL], [test x${use_curl} = xyes])

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
AC_CONFIG_FILES([bin/couch-config.tpl])
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
AC_CONFIG_FILES([src/snappy/Makefile])
AC_CONFIG_FILES([src/snappy/google-snappy/snappy-stubs-public.h])
AC_CONFIG_FILES([src/ejson/Makefile])
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
