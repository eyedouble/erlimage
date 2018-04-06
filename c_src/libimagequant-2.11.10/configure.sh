#!/usr/bin/env bash

CONFIG="config.mk"
PREFIX="/usr/local"
LIBDIR="$PREFIX/lib"
INCLUDEDIR="$PREFIX/include"
PKGCONFIGDIR="$LIBDIR/pkgconfig"
VERSION=$(grep LIQ_VERSION_STRING libimagequant.h | grep -Eo "2\.[0-9.]+")

DEBUG=
QUIET=0
SSE=auto
OPENMP=
EXTRA_CFLAGS=
EXTRA_LDFLAGS=

# make gcc default compiler unless CC is already set
CC=${CC:-gcc}

help() {
    printf "%4s %s\n" "" "$1"
}

for i in "$@"; do
    case $i in
    --help|-h)
        echo
        help "--prefix=<dir>                installation directory [$PREFIX]"
        help "--libdir=<dir>                installation directory [$LIBDIR]"
        help "--includedir=<dir>            installation directory [$INCLUDEDIR]"
        help "--pkgconfigdir=<dir>          installation directory [$PKGCONFIGDIR]"
        help "--extra-cflags=<flags>        append to CFLAGS"
        help "--extra-ldflags=<flags>       append to LDFLAGS"
        echo
        help "--enable-debug"
        help "--enable-sse/--disable-sse    enable/disable SSE instructions"
        echo
        help "--with-openmp=static          compile with multicore support"
        echo
        help "CC=<compiler>                 use given compiler command"
        help "CFLAGS=<flags>                pass options to the compiler"
        help "LDFLAGS=<flags>               pass options to the linker"
        echo
        exit 0
        ;;
    # Can be set before or after configure. Latter overrides former.
    CC=*)
        CC=${i#*=}
        ;;
    CFLAGS=*)
        CFLAGS=${i#*=}
        ;;
    LDFLAGS=*)
        LDFLAGS=${i#*=}
        ;;
    --enable-debug)
        DEBUG=1
        ;;
    --enable-sse)
        SSE=1
        ;;
    --disable-sse)
        SSE=0
        ;;
    --quiet)
        QUIET=1
        ;;
    '')
        # allows a bash quirk
        ;;
    --with-openmp)
        OPENMP=1
        ;;
    --with-openmp=static)
        OPENMP=static
        ;;
    --prefix=*)
        PREFIX=${i#*=}
        LIBDIR="$PREFIX/lib"
        INCLUDEDIR="$PREFIX/include"
        PKGCONFIGDIR="$LIBDIR/pkgconfig"
        ;;
    --libdir=*)
        LIBDIR=${i#*=}
        PKGCONFIGDIR="$LIBDIR/pkgconfig"
        ;;
    --includedir=*)
        INCLUDEDIR=${i#*=}
        ;;
    --pkgconfigdir=*)
        PKGCONFIGDIR=${i#*=}
        ;;
    # can be used multiple times or in quotes to set multiple flags
    --extra-cflags=*)
        EXTRA_CFLAGS="$EXTRA_CFLAGS ${i#*=}"
        ;;
    --extra-ldflags=*)
        EXTRA_LDFLAGS="$EXTRA_LDFLAGS ${i#*=}"
        ;;
    *)
        echo "warning: unknown switch '${i%%=*}' (see $0 --help for the list)"
        ;;
    esac
done

# If someone runs sudo make install as very first command, and configure later,
# $CONFIG cannot be overwritten, and must be deleted before continuing.
if [[ -f "$CONFIG" && ! -w "$CONFIG" ]]; then
    echo "Cannot overwrite file $CONFIG! Please delete it."
    exit 1
fi

cflags() {
    CFLAGS="$CFLAGS $1"
}

lflags() {
    LDFLAGS="$LDFLAGS $1"
}

status() {
    if [ "$QUIET" -ne 1 ]; then
        printf "%10s: %s\n" "$1" "$2"
    fi
}

# Append to CFLAGS if compiler supports flag, with optional prerequisite.
# Fails on errors and warnings.
conditional_cflags() {
    if [ -z "$(echo | "$CC" -xc -S -o /dev/null $2 $1 - 2>&1)" ]; then
        cflags "$1"
    fi
}

error() {
    status "$1" "error ... $2"
    echo
    exit 1
}

if [ "$QUIET" -ne 1 ]; then
    echo
fi

# /tmp, because mingw has problems opening /dev/null and gives false negative
if ! echo "int main(){}" | "$CC" -xc -std=c99 -o /tmp/gcccheck - > /dev/null; then
    error "Compiler" "$CC failed to compile anything (make sure it's installed and supports C99)"
fi

status "Compiler" "$CC"

# init flags
CFLAGS=${CFLAGS:--fno-math-errno -funroll-loops -fomit-frame-pointer -Wall}
cflags "-std=c99 -I."

# DEBUG
if [ -z "$DEBUG" ]; then
    cflags "-O3 -DNDEBUG"
    status "Debug" "no"
else
    cflags "-O1 -g"
    status "Debug" "yes"
fi

# SSE
if [ "$SSE" = 'auto' ]; then
    SSE=0
    if type uname > /dev/null; then
        if [[ "$(uname -m)" =~ "amd64" || "$(uname -m)" =~ "x86_64" ||
              "$(grep -E -m1 "^flags" /proc/cpuinfo)" =~ "sse" ]]; then
            SSE=1
        fi
    fi
fi

if [ "$SSE" -eq 1 ]; then
    status "SSE" "yes"
    cflags "-DUSE_SSE=1"
    cflags "-msse"
    # Silence a later ICC warning due to -msse working slightly different.
    conditional_cflags "-wd10121"
    # Must be set explicitly for GCC on x86_32. Other compilers imply it.
    conditional_cflags "-mfpmath=sse" "-msse"
elif [ "$SSE" -eq 0 ]; then
    status "SSE" "no"
    cflags "-DUSE_SSE=0"
fi

# OpenMP
if [ -n "$OPENMP" ]; then
    if [ "static" = "$OPENMP" ]; then
        OPENMPFLAGS="-static-libgcc -Bstatic -fopenmp -Bdynamic"
    else
        OPENMPFLAGS="-fopenmp"
    fi
    if [[ "$("$CC" -xc -E $OPENMPFLAGS <(echo "#ifdef _OPENMP
           #include <omp.h>
           #endif") 2>&1)" =~ "omp_get_thread_num" ]]; then
        cflags "$OPENMPFLAGS"
        lflags "$OPENMPFLAGS"
        status "OpenMP" "yes"
    else
        error "OpenMP" "not supported by compiler (please install a compiler that supports OpenMP (e.g. gcc) and specify it with the CC= argument)"
    fi
else
    # silence warnings about omp pragmas
    cflags "-Wno-unknown-pragmas"
    conditional_cflags "-wd3180" # ICC
    status "OpenMP" "no"
fi

# Cocoa
if [[ "$OSTYPE" =~ "darwin" ]]; then
    cflags "-mmacosx-version-min=10.6"
    lflags "-mmacosx-version-min=10.6"
fi

if [[ "$OSTYPE" =~ "darwin" ]]; then
    SOLIBSUFFIX=dylib

    # Search Developer SDK paths, since Apple seems to have dropped the standard Unixy ones
    XCODE_CMD="xcode-select"
    XCODE_PATH=$($XCODE_CMD -p)
    DIRS+=("$XCODE_PATH/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/usr/include $XCODE_PATH/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk/usr/lib")
    DIRS+=("$XCODE_PATH/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk/usr/include $XCODE_PATH/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk/usr/lib")
elif [[ "$OSTYPE" =~ "msys" ]]; then
    SOLIBSUFFIX=dll
else
    SOLIBSUFFIX=so
fi

if [ "$QUIET" -ne 1 ]; then
    echo
fi

# As of GCC 4.5, 387 fp math is significantly slower in C99 mode without this.
# Note: CPUs without SSE2 use 387 for doubles, even when SSE fp math is set.
conditional_cflags "-fexcess-precision=fast"

# Intel C++ Compiler

# ICC does usually only produce fast(er) code when it can optimize to the full
# capabilites of the (Intel) CPU. This is equivalent to -march=native for GCC.
conditional_cflags "-xHOST"

# Disable unsafe fp optimizations and enforce fp precision as set in the source.
conditional_cflags "-fp-model source"

# Silence a gold linker warning about string misalignment.
conditional_cflags "-falign-stack=maintain-16-byte"

lflags "-lm" # Ubuntu requires this library last, issue #38

if [ -n "$EXTRA_CFLAGS" ]; then
    cflags "$EXTRA_CFLAGS"
fi

if [ -n "$EXTRA_LDFLAGS" ]; then
    lflags "$EXTRA_LDFLAGS"
fi

# Overwrite previous configuration.
echo "
# auto-generated by configure
PREFIX = $PREFIX
LIBDIR = $LIBDIR
INCLUDEDIR = $INCLUDEDIR
PKGCONFIGDIR = $PKGCONFIGDIR
VERSION = $VERSION
CC = $CC
CFLAGS = $CFLAGS
LDFLAGS = $LDFLAGS
SOLIBSUFFIX = $SOLIBSUFFIX
" > "$CONFIG"
