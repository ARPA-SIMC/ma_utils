AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I$(top_builddir)/modules -DPKGDATAROOTDIR='"$(datarootdir)/$(PACKAGE)"' -cpp
LDADD = $(top_builddir)/modules/libmautils.la

pkglibexec_PROGRAMS = append_calmet_dat.exe asc2calmet_dat.exe calc_staz_dist.exe calmet2grib.exe calmet_dat2asc.exe crea_calmet_inp.exe crea_date_calmet.exe crea_prq_surf.exe  crea_wt_dat.exe crea_surf_dat.exe crea_surf_req.exe dummy_grib_calmet.exe extrap_wind.exe geo_dat2grib.exe grib23ddat.exe grib2mm5.exe grib2up_dat.exe proc_seriet_prf.exe proc_seriet_surf.exe proc_st1a.exe rw_dat_calmet.exe rw_xout_calmet.exe sel_staz_calmet.exe test_inp_calmet.exe xout2grib.exe

append_calmet_dat_exe_SOURCES = append_calmet_dat.f90
asc2calmet_dat_exe_SOURCES = asc2calmet_dat.f90
calc_staz_dist_exe_SOURCES = calc_staz_dist.f90
calmet2grib_exe_SOURCES = calmet2grib.f90
calmet_dat2asc_exe_SOURCES = calmet_dat2asc.f90
crea_calmet_inp_exe_SOURCES = crea_calmet_inp.f90
crea_date_calmet_exe_SOURCES = crea_date_calmet.f90
crea_prq_surf_exe_SOURCES = crea_prq_surf.f90
crea_wt_dat_exe_SOURCES = crea_wt_dat.f90
crea_surf_dat_exe_SOURCES = crea_surf_dat.f90
crea_surf_req_exe_SOURCES = crea_surf_req.f90
dummy_grib_calmet_exe_SOURCES = dummy_grib_calmet.f90
extrap_wind_exe_SOURCES = extrap_wind.f90
geo_dat2grib_exe_SOURCES = geo_dat2grib.f90
grib23ddat_exe_SOURCES = grib23ddat.f90
grib2mm5_exe_SOURCES = grib2mm5.f90
grib2up_dat_exe_SOURCES = grib2up_dat.f90
proc_seriet_prf_exe_SOURCES = proc_seriet_prf.f90
proc_seriet_surf_exe_SOURCES = proc_seriet_surf.f90
proc_st1a_exe_SOURCES = proc_st1a.f90
rw_dat_calmet_exe_SOURCES = rw_dat_calmet.f90
rw_xout_calmet_exe_SOURCES = rw_xout_calmet.f90
sel_staz_calmet_exe_SOURCES = sel_staz_calmet.f90
test_inp_calmet_exe_SOURCES = test_inp_calmet.f90
xout2grib_exe_SOURCES = xout2grib.f90

CLEANFILES = local.mod
