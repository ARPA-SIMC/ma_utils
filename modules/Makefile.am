noinst_LTLIBRARIES = libmautils.la

AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I/usr/lib/gfortran/modules -DPKGDATAROOTDIR='"$(datarootdir)/$(PACKAGE)"' -cpp

libmautils_la_SOURCES = \
	date_handler.f90 \
	grib2_utilities.f90 \
	grid_handler.f90 \
	seriet_utilities.f90

CLEANFILES = \
	date_handler.mod \
	grib2_utilities.mod \
	grid_handler.mod \
	seriet_utilities.mod
