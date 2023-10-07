AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I$(top_builddir)/modules
LDADD = $(top_builddir)/modules/libmautils.la

pkglibexec_PROGRAMS = adjust_ctl.exe convert_shape.exe coord2marks.exe
## Programs that call emoslib: grib_utm_scale.exe

adjust_ctl_exe_SOURCES = adjust_ctl.f90
convert_shape_exe_SOURCES = convert_shape.f90
grib_utm_scale_exe_SOURCES = grib_utm_scale.f90
coord2marks_exe_SOURCES = coord2marks.f90
