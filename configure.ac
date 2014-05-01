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

AC_INIT(
    [LOCAL_PACKAGE_NAME],
    [LOCAL_VERSION],
    [LOCAL_BUG_URI],
    [LOCAL_PACKAGE_TARNAME])

AC_PREREQ([2.63])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_HEADERS([src/snappy/google-snappy/config.h])

AM_INIT_AUTOMAKE([1.6.3 foreign tar-ustar])

AC_USE_SYSTEM_EXTENSIONS
AC_ENABLE_SHARED
AC_DISABLE_STATIC

AC_PROG_CC
LT_INIT([win32-dll])
LT_INIT
AC_PROG_LN_S

PKG_PROG_PKG_CONFIG

dnl Config for google snappy
m4_define([snappy_major], [1])
m4_define([snappy_minor], [0])
m4_define([snappy_patchlevel], [5])

AC_PROG_CXX
AC_LANG([C++])
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],,
    AC_MSG_ERROR([A C++ compiler is required.]))
AC_C_BIGENDIAN
AC_CHECK_HEADERS([stdint.h stddef.h sys/mman.h sys/resource.h])
AC_CHECK_FUNC([mmap])

AC_MSG_CHECKING([if the compiler supports __builtin_expect])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    return __builtin_expect(1, 1) ? 1 : 0
]])],[
    snappy_have_builtin_expect=yes
    AC_MSG_RESULT([yes])
],[
    snappy_have_builtin_expect=no
    AC_MSG_RESULT([no])
])

if test x$snappy_have_builtin_expect = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_EXPECT], [1],
        [Define to 1 if the compiler supports __builtin_expect.])
fi

AC_MSG_CHECKING([if the compiler supports __builtin_ctzll])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[
    return (__builtin_ctzll(0x100000000LL) == 32) ? 1 : 0
]])],[
    snappy_have_builtin_ctz=yes
    AC_MSG_RESULT([yes])
],[
    snappy_have_builtin_ctz=no
    AC_MSG_RESULT([no])
])

if test x$snappy_have_builtin_ctz = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_CTZ], [1],
        [Define to 1 if the compiler supports __builtin_ctz and friends.])
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

AC_MSG_CHECKING([for pthread_create in -lpthread])

original_LIBS="$LIBS"
LIBS="-lpthread $original_LIBS"

AC_LINK_IFELSE([
    AC_LANG_PROGRAM(
        [[#include<pthread.h>]],
        [[pthread_create((void *)0, (void *)0, (void *)0, (void *)0)]])],
        [pthread=yes],
        [pthread=no])

if test x${pthread} = xyes; then
    AC_MSG_RESULT([yes])
else
    LIBS="$original_LIBS"
    AC_MSG_RESULT([no])
fi

AC_PATH_PROG([ERL], [erl])
AC_PATH_PROG([ESCRIPT], [escript])

AS_IF([test x${ERL} = x], [
    AC_MSG_ERROR([Could not find the `erl' executable. Is Erlang installed?])
])

AC_ARG_WITH([erlang],
    [AS_HELP_STRING([--with-erlang=PATH],
        [set PATH to the Erlang include directory])
], [
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

PKG_CHECK_MODULES([JS], [mozjs185], [
    JS_LIB_DIR="$(${PKG_CONFIG} --variable=libdir mozjs185)"
], [
    PKG_CHECK_MODULES([JS], [mozilla-js >= 1.7], [
        JS_LIB_DIR="$(${PKG_CONFIG} --variable=sdkdir mozilla-js)/lib"
    ], [
        JS_LIB_DIR="${libdir}"
        JS_CFLAGS="-I/usr/include"
        JS_CFLAGS="$JS_CFLAGS -I/usr/include/js"
        JS_CFLAGS="$JS_CFLAGS -I/usr/include/mozjs"
        JS_CFLAGS="$JS_CFLAGS -I/usr/local/include/js"
        JS_CFLAGS="$JS_CFLAGS -I/opt/local/include/js"
    ])
])

AC_ARG_WITH([js-include],
    [AS_HELP_STRING([--with-js-include=PATH],
        [set PATH to the SpiderMonkey include directory])
], [
    JS_INCLUDE="$withval"
    JS_CFLAGS="-I$JS_INCLUDE"
], [])

AC_ARG_WITH([js-lib],
    [AS_HELP_STRING([--with-js-lib=PATH],
        [set PATH to the SpiderMonkey library directory])
], [
    JS_LIB_DIR=$withval
    JS_LIBS="-L$withval"
], [])

use_js_trunk=no

AC_ARG_ENABLE([js-trunk],
    [AS_HELP_STRING([--enable-js-trunk],
        [allow use of SpiderMonkey versions newer than js185-1.0.0])
], [
    use_js_trunk=$enableval
], [])

AC_ARG_VAR([ERLC_FLAGS], [general flags to prepend to ERLC_FLAGS])
AC_ARG_VAR([FLAGS], [general flags to prepend to LDFLAGS and CPPFLAGS])

AS_CASE([$(uname -s)], [CYGWIN*], [] , [*], [
    CPPFLAGS="$CPPFLAGS -I/opt/local/include"
    CPPFLAGS="$CPPFLAGS -I/opt/local/include/js"
    CPPFLAGS="$CPPFLAGS -I/usr/local/include"
    CPPFLAGS="$CPPFLAGS -I/usr/local/include/js"
    CPPFLAGS="$CPPFLAGS -I/usr/include"
    CPPFLAGS="$CPPFLAGS -I/usr/include/js"
    LDFLAGS="$LDFLAGS -L/opt/local/lib"
    LDFLAGS="$LDFLAGS -L/usr/local/lib"
])

CPPFLAGS="$CPPFLAGS $FLAGS"
LDFLAGS="$LDFLAGS $FLAGS"

# The erlang cc.sh/ld.sh scripts will convert a -O option
# into the same optimization flags erlang itself uses.
CFLAGS="-O2 $CFLAGS"

LIBS="$LIBS $LDFLAGS"

AS_CASE([$(uname -s)], [CYGWIN*], [
    JS_CFLAGS="-DXP_WIN $JS_CFLAGS"
    IS_WINDOWS="TRUE"
], [*], [
    # XP_UNIX required for jsapi.h, tested on Linux and Darwin.
    JS_CFLAGS="-DXP_UNIX $JS_CFLAGS"
])

AM_CONDITIONAL([WINDOWS], [test x$IS_WINDOWS = xTRUE])

OLD_LIBS="$LIBS"
LIBS="$JS_LIBS $LIBS"

OLD_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$JS_CFLAGS $CPPFLAGS"

AC_CHECK_HEADER([jsapi.h], [], [
    AC_CHECK_HEADER([js/jsapi.h], [
        CPPFLAGS="$CPPFLAGS -I$JS_INCLUDE/js"
    ], [
        AC_MSG_ERROR([Could not find the jsapi header.

Are the Mozilla SpiderMonkey headers installed?])
    ])
])

AC_CHECK_LIB([mozjs185], [JS_NewContext], [JS_LIB_BASE=mozjs185], [
    AC_CHECK_LIB([mozjs185-1.0], [JS_NewContext], [JS_LIB_BASE=mozjs185-1.0], [
        AC_CHECK_LIB([mozjs], [JS_NewContext], [JS_LIB_BASE=mozjs], [
            AC_CHECK_LIB([js], [JS_NewContext], [JS_LIB_BASE=js], [
                AC_CHECK_LIB([js3250], [JS_NewContext], [JS_LIB_BASE=js3250], [
                    AC_CHECK_LIB([js32], [JS_NewContext], [JS_LIB_BASE=js32], [
                        AC_MSG_ERROR([Could not find the js library.

Is the Mozilla SpiderMonkey library installed?])
                    ])
                ])
            ])
        ])
    ])
])

# Figure out what version of SpiderMonkey to use

AC_CHECK_LIB([$JS_LIB_BASE], [JS_NewCompartmentAndGlobalObject],
    # Prevent people from accidentally using SpiderMonkey's that are too new
    if test "$use_js_trunk" = "no"; then
        AC_CHECK_DECL([JSOPTION_ANONFUNFIX], [], [
            AC_MSG_ERROR([Your SpiderMonkey library is too new.

Versions of SpiderMonkey after the js185-1.0.0 release remove the optional
enforcement of preventing anonymous functions in a statement context. This
will most likely break your existing JavaScript code as well as render all
example code invalid.

If you wish to ignore this error pass --enable-js-trunk to ./configure.])
        ], [
            [#include <jsapi.h>]
        ])
    fi
    AC_DEFINE([SM185], [1], [Use SpiderMonkey 1.8.5])
)

AC_CHECK_LIB([$JS_LIB_BASE], [JS_ThrowStopIteration],
    AC_DEFINE([SM180], [1],
        [Use SpiderMonkey 1.8.0])
)

AC_CHECK_LIB([$JS_LIB_BASE], [JS_GetStringCharsAndLength],
    AC_DEFINE([HAVE_JS_GET_STRING_CHARS_AND_LENGTH], [1],
        [Use newer JS_GetCharsAndLength function.])
)

# Else, hope that 1.7.0 works

# Deal with JSScript to JSObject to JSScript switcheroo

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
    AC_ARG_WITH([openssl-bin-dir],
        [AS_HELP_STRING([--with-openssl-bin-dir=PATH],
            [path to the open ssl binaries for distribution on Windows])
    ], [
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
    AC_ARG_WITH([msvc-redist-dir],
        [AS_HELP_STRING([--with-msvc-redist-dir=PATH],
            [path to the msvc redistributables for the Windows platform])
    ], [
        msvc_redist_dir=`cygpath -m "$withval"`
        msvc_redist_name="vcredist_x86.exe"
        AC_SUBST(msvc_redist_dir)
        AC_SUBST(msvc_redist_name)
    ], [])
    if test ! -f ${msvc_redist_dir}/${msvc_redist_name}; then
        AC_MSG_WARN([Installer may fail due to missing MSVC redistributable.])
    fi
fi

JS_LIBS="-l$JS_LIB_BASE -lm $JS_LIBS"

AC_SUBST(JS_LIBS)

LIBS="$OLD_LIBS"
CPPFLAGS="$OLD_CPPFLAGS"

%% auto detect "kegged" icu4c on Mac / Homebrew
PATH="/usr/local/opt/icu4c/bin:$PATH"
AC_ARG_WITH([win32-icu-binaries],
    [AS_HELP_STRING([--with-win32-icu-binaries=PATH],
        [set PATH to the Win32 native ICU binaries directory])
], [
    ICU_CPPFLAGS="-I$withval/include"
    ICU_LIBS="-L$withval/lib -licuuc -licudt -licuin"
    ICU_BIN=$withval/bin
], [
    AX_CHECK_ICU([3.4.1])
    ICU_BIN=
])

AC_SUBST(ICU_CFLAGS)
AC_SUBST(ICU_CPPFLAGS)
AC_SUBST(ICU_LIBS)
AC_SUBST(ICU_BIN)

use_curl=yes

AC_ARG_WITH([win32-curl],
    [AS_HELP_STRING([--with-win32-curl=PATH],
        [set PATH to the Win32 native curl directory])
], [
    # default build on windows is a static lib, and that's what we want too
    CURL_CFLAGS="-I$withval/include -DCURL_STATICLIB"
    CURL_LIBS="-L$withval/lib -llibcurl -lWs2_32 -lkernel32 -luser32 -ladvapi32 -lWldap32"
    # OpenSSL libraries may be pulled in via libcurl if it was built with SSL
    # these are libeay32 ssleay32 instead of crypto ssl on unix
], [
    AX_LIB_CURL([7.18.0],[
        AC_DEFINE([HAVE_CURL], [1], ["Provide HTTP support to couchjs"])
    ],[
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


{ $as_echo "$as_me:${as_lineno-$LINENO}: checking Erlang version compatibility" >&5
$as_echo_n "checking Erlang version compatibility... " >&6; }
erlang_version_error="The installed Erlang version must be >= R14B (erts-5.8.1) and =< 17 (erts-6.0)"

version="`${ERL} -version 2>&1 | ${SED} 's/[[^0-9]]/ /g'` 0 0 0"
major_version=`echo $version | ${AWK} "{print \\$1}"`
minor_version=`echo $version | ${AWK} "{print \\$2}"`
patch_version=`echo $version | ${AWK} "{print \\$3}"`
echo -n "detected Erlang version: $major_version.$minor_version.$patch_version..."

if test $major_version -lt 5 -o $major_version -gt 6; then
    as_fn_error $? "$erlang_version_error major_version does not match" "$LINENO" 5
fi

if test $major_version -eq 5 -a $minor_version -lt 8; then
    as_fn_error $? "$erlang_version_error minor_version does not match" "$LINENO" 5
fi

AC_MSG_RESULT(compatible)

# returns 17 for erts-6.0, and R14B03 or similar for earlier releases
otp_release="`\
    ${ERL} -noshell \
    -eval 'io:put_chars(erlang:system_info(otp_release)).' \
    -s erlang halt`"

AC_SUBST(otp_release)

AM_CONDITIONAL([USE_OTP_NIFS],
    [can_use_nifs=$(echo $otp_release | grep -E "^(R14B|R15|R16|17)")])
AM_CONDITIONAL([USE_EJSON_COMPARE_NIF],
    [can_use_ejson=$(echo $otp_release | grep -E "^(R14B0[[34]]|R15|R16|17)")])

has_crypto=`\
    ${ERL} -eval "\
        case application:load(crypto) of ok -> ok; _ -> exit(no_crypto) end. \
    " -noshell -s init stop`

if test -n "$has_crypto"; then
    AC_MSG_ERROR([Could not find the Erlang crypto library.
    
Has Erlang been compiled with OpenSSL support?])
fi

AC_PATH_PROG([ERLC], [erlc])

if test x${ERLC} = x; then
    AC_MSG_ERROR([Could not find the `erlc' executable.
    
Is Erlang installed?])
fi

OLD_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$ERLANG_FLAGS $CPPFLAGS"

AC_CHECK_HEADER([erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the `erl_driver.h' header.

Are the Erlang headers installed?

Use the `--with-erlang' option to specify the Erlang include directory.])
])

CPPFLAGS="$OLD_CPPFLAGS"

use_init=yes
use_launchd=yes
native_mochijson_enabled=no
tests_enabled=yes
docs_enabled=yes
strictness_enabled=no

AC_ARG_ENABLE([init],
    [AS_HELP_STRING([--disable-init],
        [don't install init script where applicable])
], [
    use_init=$enableval
], [])

AC_ARG_ENABLE([launchd],
    [AS_HELP_STRING([--disable-launchd],
        [don't install launchd configuration where applicable])
], [
    use_launchd=$enableval
], [])

AC_ARG_ENABLE([native-mochijson],
    [AS_HELP_STRING([--enable-native-mochijson],
        [compile mochijson to native code (EXPERIMENTAL)])
], [
    native_mochijson_enabled=$enableval
], [])

AC_ARG_ENABLE([tests],
    [AS_HELP_STRING([--disable-tests],
        [skip tests during build])
], [
    tests_enabled=$enableval
], [])

AC_ARG_ENABLE([docs],
    [AS_HELP_STRING([--disable-docs],
        [skip docs during build])
], [
    docs_enabled=$enableval
], [])

AC_ARG_ENABLE([strictness],
    [AS_HELP_STRING([--enable-strictness],
        [exit when optional checks fail])
], [
    strictness_enabled=$enableval
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

AC_CHECK_PROG([HAS_HELP2MAN], [help2man], [yes])

if test x${HAS_HELP2MAN} = x; then
    if test x${strictness_enabled} = xyes; then
        AC_MSG_ERROR([Could not find the `help2man' executable.])
    else
        AC_MSG_WARN([You will be unable to regenerate man pages.])
    fi
fi

AC_CHECK_PROG([HAS_PDFLATEX], [pdflatex], [yes])

if test x${HAS_PDFLATEX} = x; then
    if test x${strictness_enabled} = xyes; then
        AC_MSG_ERROR([Could not find the `pdflatex' executable.
    
Is LaTeX installed?])
    else
        AC_MSG_WARN([You will be unable to regenerate PDF documentation.])
    fi
fi

AC_CHECK_PROG([HAS_MAKEINFO], [makeinfo], [yes])

if test x${HAS_MAKEINFO} = x; then
    if test x${strictness_enabled} = xyes; then
        AC_MSG_ERROR([Could not find the `makeinfo' executable.
    
Is GNU Texinfo installed?])
    else
        AC_MSG_WARN([You will be unable to regenerate info documentation.])
    fi
fi

AC_CHECK_PROG([HAS_INSTALLINFO], [install-info], [yes])

if test x${HAS_INSTALLINFO} = x; then
    if test x${strictness_enabled} = xyes; then
        AC_MSG_ERROR([Could not find the `install-info' executable.
    
Is GNU Texinfo installed?])
    else
        AC_MSG_WARN([You will be unable to install info documentation.])
    fi
fi

AC_CHECK_PROG([HAS_SPHINX_BUILD], [sphinx-build], [yes])

if test x${HAS_SPHINX_BUILD} = x; then
    if test x${strictness_enabled} = xyes; then
        AC_MSG_ERROR([Could not find the `sphinx-build' executable.])
    else
        AC_MSG_WARN([You will be unable to regenerate documentation.])
    fi
fi

if test x${docs_enabled} = xyes; then
    if test x${HAS_HELP2MAN} = xyes; then
        build_man=yes
    fi
    if test x${HAS_SPHINX_BUILD} = xyes; then
        build_html=yes
        if test x${HAS_MAKEINFO} = xyes; then
            build_info=yes
        fi
        if test x${HAS_PDFLATEX} = xyes; then
            build_pdf=yes
        fi
    fi
fi

if test x${strictness_enabled} = xyes; then
    AX_PYTHON_MODULE([pygments], [fatal])
else
    AX_PYTHON_MODULE([pygments])
fi

AC_MSG_CHECKING(pygments version >= 1.5)

python 2> /dev/null << EOF
import sys
import pygments

if float(pygments.__version__) >= 1.5:
    sys.exit(0)
else:
    sys.exit(1)
EOF

if test $? -eq 0; then
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)
    if test x${strictness_enabled} = xyes; then
        AC_MSG_ERROR([Your copy of pygments is out of date.], 1)
    else
        AC_MSG_WARN([Syntax highlighting may not work.])
    fi
fi

ERL_VERSION_COMMAND="${ESCRIPT} bin/erlang-version.escript"
erlangversion=`${ERL_VERSION_COMMAND}`

AC_ARG_VAR([ERL], [path to the `erl' executable])
AC_ARG_VAR([ERLC], [path to the `erlc' executable])

AM_CONDITIONAL([INIT], [test x${init_enabled} = xtrue])
AM_CONDITIONAL([LAUNCHD], [test x${launchd_enabled} = xtrue])
AM_CONDITIONAL([USE_NATIVE_MOCHIJSON], [test x${native_mochijson_enabled} = xyes])
AM_CONDITIONAL([USE_CURL], [test x${use_curl} = xyes])

AM_CONDITIONAL([BUILD_MAN], [test x${build_man} = xyes])
AM_CONDITIONAL([BUILD_INFO], [test x${build_info} = xyes])
AM_CONDITIONAL([BUILD_PDF], [test x${build_pdf} = xyes])
AM_CONDITIONAL([BUILD_HTML], [test x${build_html} = xyes])

AM_CONDITIONAL([TESTS], [test x${tests_enabled} = xyes])
AM_CONDITIONAL([STRICTNESS], [test x${strictness_enabled} = xyes])

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
AC_SUBST([erlangversion], [${erlangversion}])
AC_SUBST([erlangbin], [${ERL}])


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
AC_CONFIG_FILES([share/doc/Makefile])
AC_CONFIG_FILES([share/doc/build/Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/couch_dbupdates/Makefile])
AC_CONFIG_FILES([src/couch_index/Makefile])
AC_CONFIG_FILES([src/couch_mrview/Makefile])
AC_CONFIG_FILES([src/couch_plugins/Makefile])
AC_CONFIG_FILES([src/couch_replicator/Makefile])
AC_CONFIG_FILES([src/couchjs-node/Makefile])
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
#
# As this is only done once at first configure, and subsequent config or source
# changes may trigger a silent reversion to the non-functioning original.
#
# Changes are;
#
# 1. replace LIB$name with $name in libname_spec (e.g. libicu -> icu) to ensure
#    correct windows versions of .lib and .dlls are found or generated.
# 2. remove incompatible \w-link\w from archive_cmds
# 3. remove GNU-style directives to be passed through to the linker
# 4. swap GNU-style shared library flags with MS -dll variant
#
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
