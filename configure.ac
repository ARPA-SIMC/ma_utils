#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(ma_utils, 0.1, dbranchini@arpa.emr.it)
LT_INIT
AM_INIT_AUTOMAKE([-Wall foreign])
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_PROG_FC
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for libraries.
AM_PROG_LIBTOOL

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
#AC_CHECK_LIB([grib_api],[grib_values_check])
LIBS="-lgrib_api -lgrib_api_f90 -lgrib_util -lemos -lsim_base -lsim_volgrid6d  -lsim_vol7d -lnetcdf -lnetcdff"

AC_CONFIG_FILES([Makefile
		modules/Makefile
		arkimet/src/Makefile
		calmet/src/Makefile
		chimere/src/ver2007/Makefile
		chimere/src/ver2011/Makefile
		chimere/src/Makefile])

#AC_CONFIG_FILES([Makefile

#		chimere/src/ver2011/Makefile
#		chimere/src/ver2007/tools/Makefile


#		arkimet/src/Makefile
#		calmet/src/Makefile
#		chimere/src/Makefile
#		fisiog/src/Makefile
#		osservazioni/src/Makefile
#		post_lm/src/Makefile
#		util/grads/src/Makefile
#		util/grib/src/Makefile])
AC_OUTPUT
