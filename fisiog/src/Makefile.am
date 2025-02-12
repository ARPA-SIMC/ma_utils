AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I$(top_builddir)/modules
LDADD = $(top_builddir)/modules/libmautils.la

if SMND_BUILD
pkglibexec_PROGRAMS = 
else
pkglibexec_PROGRAMS = geo_rotated.exe geo_utm.exe
endif

## Programs that call emoslib: proc_fisiog.exe calc_tpi.exe

proc_fisiog_exe_SOURCES = proc_fisiog.f90
calc_tpi_exe_SOURCES = calc_tpi.f90
geo_rotated_exe_SOURCES = geo_rotated.f90
geo_utm_exe_SOURCES = geo_utm.f90

CLEANFILES = local.mod
