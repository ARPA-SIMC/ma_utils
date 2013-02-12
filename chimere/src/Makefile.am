AM_FCFLAGS = -I/usr/include -I/usr/lib64/gfortran/modules -I/usr/lib/gfortran/modules -I$(top_builddir)/modules -I$(top_builddir)/chimere/src/ver2011 -I$(top_builddir)/chimere/src/ver2007
AM_LDFLAGS = $(top_builddir)/modules/libmautils.la

pkglibexec_PROGRAMS = altipres_sim.exe chimere2grib.exe chimeredep2grib.exe chimerencdf2grib.exe chmbc2cdf.exe crea_coord_chimere.exe exdom_grib.exe filtra_spec_out.exe grb_chimere2netcdf.exe grib2chimere.exe lm2ncf.exe merge_unf_chimere.exe mix_ratio.exe pres_ecmwf.exe scale_unf_chimere.exe sum_unf_chimere.exe show_unf_chimere.exe riscrivi_meteonc.exe split_output.exe sum_pm_species.exe tinterp_sim.exe diagmet_LMSMR4.exe diagmet_LAMAZ.exe diagmet_LAMA3.exe

altipres_sim_exe_SOURCES = altipres_sim.f90      
chimere2grib_exe_SOURCES = chimere2grib.f90      
chimeredep2grib_exe_SOURCES = chimeredep2grib.f90   

chimerencdf2grib_exe_SOURCES = chimerencdf2grib.F90
chimerencdf2grib_exe_LDADD = $(top_builddir)/chimere/src/ver2011/libchimere2011.la

chmbc2cdf_exe_SOURCES = chmbc2cdf.F90
chmbc2cdf_exe_LDADD = $(top_builddir)/chimere/src/ver2011/libchimere2011.la

crea_coord_chimere_exe_SOURCES = crea_coord_chimere.f90
exdom_grib_exe_SOURCES = exdom_grib.f90	      
filtra_spec_out_exe_SOURCES = filtra_spec_out.f90   
grb_chimere2netcdf_exe_SOURCES = grb_chimere2netcdf.f90
grib2chimere_exe_SOURCES = grib2chimere.f90      

lm2ncf_exe_SOURCES = lm2ncf.F90
lm2ncf_exe_LDADD = $(top_builddir)/chimere/src/ver2011/libchimere2011.la

merge_unf_chimere_exe_SOURCES = merge_unf_chimere.f90 
mix_ratio_exe_SOURCES = mix_ratio.f90
pres_ecmwf_exe_SOURCES = pres_ecmwf.f90
scale_unf_chimere_exe_SOURCES = scale_unf_chimere.f90
sum_unf_chimere_exe_SOURCES = sum_unf_chimere.f90
show_unf_chimere_exe_SOURCES = show_unf_chimere.f90

riscrivi_meteonc_exe_SOURCES = riscrivi_meteonc.F90
riscrivi_meteonc_exe_LDADD = $(top_builddir)/chimere/src/ver2011/libchimere2011.la

split_output_exe_SOURCES = split_output.f90      
sum_pm_species_exe_SOURCES = sum_pm_species.f90    
tinterp_sim_exe_SOURCES = tinterp_sim.f90	      

diagmet_LMSMR4_exe_SOURCES = ver2007/modules/chimere_params.LMSMR4.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.F90 ver2007/diag/diagbio_common.F90 ver2007/diag/diagmet_science.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.F90
diagmet_LMSMR4_exe_LDADD = $(top_builddir)/chimere/src/ver2007/libchimere2007lmsmr4.la

diagmet_LAMAZ_exe_SOURCES = ver2007/modules/chimere_params.LAMAZ.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.F90 ver2007/diag/diagbio_common.F90 ver2007/diag/diagmet_science.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.F90
diagmet_LAMAZ_exe_LDADD = $(top_builddir)/chimere/src/ver2007/libchimere2007lamaz.la

diagmet_LAMA3_exe_SOURCES = ver2007/modules/chimere_params.LAMA3.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.F90 ver2007/diag/diagbio_common.F90 ver2007/diag/diagmet_science.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.F90
diagmet_LAMA3_exe_LDADD = $(top_builddir)/chimere/src/ver2007/libchimere2007lama3.la

CLEANFILES = \
	diagmet_science.mod \
	chimere_params.mod \
	chimere_consts.mod \
	diagbio_common.mod \
	diagmet_common.mod