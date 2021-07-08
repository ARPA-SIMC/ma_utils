#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(ma_utils, 0.14, dbranchini@arpae.it)

m4_pattern_allow([AM_PROG_AR])
AM_PROG_AR

LT_INIT
AM_INIT_AUTOMAKE([-Wall foreign subdir-objects])
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

grib_api_flavor=
AC_CHECK_LIB(
	     [eccodes_f90],
	     [grib_f_open_file],
	     [
	      AC_MSG_RESULT([eccodes code included])
	      LIBS="-leccodes_f90 -leccodes -ljasper"
              grib_api_flavor=eccodes
	     ],
	     [AC_MSG_RESULT([eccodes library not found, trying with legacy grib_api])],
	     [-leccodes -ljasper]
	    )
if test "x$grib_api_flavor" = "x"; then
AC_CHECK_LIB(
	     [grib_api_f90],
	     [grib_f_open_file],
	     [
	      AC_MSG_RESULT([gribapi code included])
	      LIBS="-lgrib_api_f90 -lgrib_api -ljasper"
              grib_api_flavor=grib_api
	     ],
	     [AC_MSG_ERROR([eccodes and grib-api library not found])],
	     [-lgrib_api -ljasper]
	    )
fi

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_ARG_ENABLE(smnd-build, AC_HELP_STRING([--enable-smnd-build],
  [enable smnd build]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
    LIBS="$LIBS -lsim_base -lsim_volgrid6d -lsim_vol7d"
else
    LIBS="$LIBS -lemos -lsim_base -lsim_volgrid6d  -lsim_vol7d -lnetcdf -lnetcdff"
fi

AM_CONDITIONAL(SMND_BUILD, test "x$enableval" = "xyes")

# Checks for library functions.
#AC_CHECK_LIB([grib_api],[grib_values_check])

# Debug mode for fortran condes
AC_ARG_ENABLE(debug-mode, AC_HELP_STRING([--enable-debug-mode],
  [enable debug mode]), , [enableval=no])dnl default disable
if test "x$enableval" = "xyes"; then
   if test "x$FC" = "xgfortran"; then
      FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions -Wall -pg -ffpe-trap=invalid,zero,overflow,underflow"
# the "inexact" flag has problems with WRITE(*,*) (28/04/2021, gfortran 10.2.1)
#     FCFLAGS="$FCFLAGS -g -fbounds-check -O0 -fexceptions -Wall -pg -ffpe-trap=invalid,zero,overflow,underflow,inexact"
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
