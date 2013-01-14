AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I$(top_builddir)/modules
AM_LDFLAGS = $(top_builddir)/modules/libmautils.la

bin_PROGRAMS = proc_fisiog.exe calc_tpi.exe

proc_fisiog_exe_SOURCES = proc_fisiog.f90
calc_tpi_exe_SOURCES = calc_tpi.f90

CLEANFILES = local.mod
