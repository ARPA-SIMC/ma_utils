AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I$(top_builddir)/modules
LDADD = $(top_builddir)/modules/libmautils.la

pkglibexec_PROGRAMS = calc_anh01.exe calc_ps_cosmo.exe calc_swdown.exe calc_ustar_mo.exe

## Programs that call emoslib: calc_ipgt_inv_multi.exe calc_inv_max.exe post_zi_diagmet.exe split_qrs.exe qsoil_ecmwf2lama.exe stat_qrs_temp.exe grib_ttd2rh.exe post_wind_lm.exe lm_levels2layers.exe calc_ipgt.exe correct_lamaz.exe decumula_lm.exe 

calc_anh01_exe_SOURCES = calc_anh01.f90
calc_inv_max_exe_SOURCES = calc_inv_max.f90
calc_ipgt_exe_SOURCES = calc_ipgt.f90
calc_ipgt_inv_multi_exe_SOURCES = calc_ipgt_inv_multi.f90
calc_ps_cosmo_exe_SOURCES = calc_ps_cosmo.f90
calc_swdown_exe_SOURCES = calc_swdown.f90
calc_ustar_mo_exe_SOURCES = calc_ustar_mo.f90
correct_lamaz_exe_SOURCES = correct_lamaz.f90
decumula_lm_exe_SOURCES = decumula_lm.f90
grib_ttd2rh_exe_SOURCES = grib_ttd2rh.f90
lm_levels2layers_exe_SOURCES = lm_levels2layers.f90
post_wind_lm_exe_SOURCES = post_wind_lm.f90
post_zi_diagmet_exe_SOURCES = post_zi_diagmet.f90
qsoil_ecmwf2lama_exe_SOURCES = qsoil_ecmwf2lama.f90
split_qrs_exe_SOURCES = split_qrs.f90
stat_qrs_temp_exe_SOURCES = stat_qrs_temp.f90
