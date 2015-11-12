#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(ma_utils, 0.9, dbranchini@arpa.emr.it)

m4_pattern_allow([AM_PROG_AR])
AM_PROG_AR

LT_INIT
AM_INIT_AUTOMAKE([-Wall foreign])
AC_CONFIG_MACRO_DIR([m4])


# Checks for programs.
AC_PROG_FC
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_CHECK_PROG([KSH], [ksh], [ksh])
if test "x$KSH" = "x"; then
   AC_MSG_ERROR([ksh not found])
fi

# Checks for libraries.
AM_PROG_LIBTOOL

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
#AC_CHECK_LIB([grib_api],[grib_values_check])
LIBS="-lgrib_api -lgrib_api_f90 -lgrib_util -lemos -lsim_base -lsim_volgrid6d  -lsim_vol7d -lnetcdf -lnetcdff"

# Debug mode for fortran condes
AC_ARG_ENABLE(debug-mode, AC_HELP_STRING([--enable-debug-mode],
  [enable debug mode]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
   if test "x$FC" = "xgfortran"; then
      FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions -Wall -pg"
#     FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions -Wall"
   else
      FCFLAGS="$FCFLAGS -g -C"
   fi
   AC_DEFINE(DEBUG, 1, [Enable debugging mode])
fi

AC_CONFIG_FILES([Makefile
		modules/Makefile
		arkimet/src/Makefile
		arkimet/sh/Makefile
		arkimet/postprocessors/seriet/Makefile
		calmet/src/Makefile
		calmet/sh/Makefile
		chimere/src/ver2007/Makefile
		chimere/src/ver2011/Makefile
		chimere/src/Makefile
		chimere/sh/Makefile
		fisiog/src/Makefile
		osservazioni/src/Makefile
		osservazioni/sh/Makefile
		post_lm/src/Makefile
		util/grads/src/Makefile
		util/grib/src/Makefile])

AC_OUTPUT
