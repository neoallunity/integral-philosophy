# Autoconf configuration for Integral Philosophy Publishing System
# =============================================================

AC_INIT([Integral Philosophy Publishing System], [0.1.0.0], [support@integral-philosophy.org], [integral-philosophy], [https://github.com/integral-philosophy/integral-philosophy])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

# Initialize Automake
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall -Werror parallel-tests])

# Check for programs
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_SED

# Check for Haskell tools
AC_CHECK_PROG([GHC], [ghc], [ghc], [no])
AC_CHECK_PROG([STACK], [stack], [stack], [no])

if test "$GHC" = "no"; then
    AC_MSG_ERROR([GHC (Glasgow Haskell Compiler) is required but not found])
fi

if test "$STACK" = "no"; then
    AC_MSG_ERROR[Stack build tool is required but not found]
fi

# Get GHC version
GHC_VERSION=$($GHC --version | $SED -n 's/.*version \([0-9.]*\).*/\1/p')
AC_SUBST([GHC_VERSION])

# Check for required Stack files
if test ! -f "haskell-project/package.yaml"; then
    AC_MSG_ERROR([haskell-project/package.yaml not found])
fi

if test ! -f "haskell-project/stack.yaml"; then
    AC_MSG_ERROR([haskell-project/stack.yaml not found])
fi

# Configuration directories
AC_SUBST([BUILD_DIR], ["${prefix}/build"])
AC_SUBST([BIN_DIR], ["${prefix}/bin"])
AC_SUBST([HASKELL_PROJECT_DIR], ["${srcdir}/haskell-project"])

# Set up output files
AC_CONFIG_FILES([
    Makefile
    src/Makefile
    build-aux/Makefile
])

# Optional features
AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug], [Enable debug build @<:@default=no@:>@])],
    [enable_debug=$enableval],
    [enable_debug=no])

AC_ARG_ENABLE([optimization],
    [AS_HELP_STRING([--enable-optimization], [Enable optimization @<:@default=yes@:>@])],
    [enable_optimization=$enableval],
    [enable_optimization=yes])

# Set build flags based on options
if test "$enable_debug" = "yes"; then
    GHC_FLAGS="-Wall -debug"
    STACK_FLAGS="--fast"
else
    GHC_FLAGS="-Wall -O2"
    STACK_FLAGS=""
fi

if test "$enable_optimization" = "yes"; then
    GHC_FLAGS="$GHC_FLAGS -O2"
    STACK_FLAGS="$STACK_FLAGS"
fi

AC_SUBST([GHC_FLAGS])
AC_SUBST([STACK_FLAGS])

# Test for optional dependencies
AC_ARG_WITH([test-file],
    [AS_HELP_STRING([--with-test-file=FILE], [Test LaTeX file for validation @<:@default=test.tex@:>@])],
    [TEST_FILE=$withval],
    [TEST_FILE="test.tex"])

AC_SUBST([TEST_FILE])

# Summary
AC_MSG_NOTICE([Integral Philosophy Publishing System Configuration:])
AC_MSG_NOTICE([  GHC Version: $GHC_VERSION])
AC_MSG_NOTICE([  Build Directory: $BUILD_DIR])
AC_MSG_NOTICE([  Binary Directory: $BIN_DIR])
AC_MSG_NOTICE([  Haskell Project: $HASKELL_PROJECT_DIR])
AC_MSG_NOTICE([  GHC Flags: $GHC_FLAGS])
AC_MSG_NOTICE([  Stack Flags: $STACK_FLAGS])
AC_MSG_NOTICE([  Test File: $TEST_FILE])
AC_MSG_NOTICE([  Debug Build: $enable_debug])
AC_MSG_NOTICE([  Optimization: $enable_optimization])

AC_OUTPUT