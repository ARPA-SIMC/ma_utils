AM_FCFLAGS = -I/usr/include -I/usr/lib64/gfortran/modules -I/usr/lib/gfortran/modules -I$(top_builddir)/modules -I$(top_builddir)/chimere/src/ver2011 -I$(top_builddir)/chimere/src/ver2007 -DPKGDATAROOTDIR='"$(datarootdir)/$(PACKAGE)"' -cpp
LDADD = $(top_builddir)/modules/libmautils.la

pkglibexec_PROGRAMS = altipres_sim.exe chimere_cdfluse2asc.exe chimerencdfmet2grib.exe chmbc2cdf.exe column_content.exe crea_coord_chimere.exe filtra_spec_out.exe grib2chimerencdf.exe grib2diagmetncdf.exe lm2ncf.exe merge_unf_chimere.exe mix_ratio.exe prepcosmomed.exe scale_unf_chimere.exe sum_unf_chimere.exe show_unf_chimere.exe riscrivi_meteonc.exe split_output.exe tinterp_sim.exe diagmet_LAMA5.exe diagmet_ITA5KM.exe

## Programs that call emoslib: pres_ecmwf.exe exdom_grib.exe chimeredep2grib.exe sum_pm_species.exe chimere2grib.exe grib2chimere.exe chimerencdf2grib.exe 

altipres_sim_exe_SOURCES = altipres_sim.f90      
chimere_cdfluse2asc_exe_SOURCES = chimere_cdfluse2asc.f90
chimere2grib_exe_SOURCES = chimere2grib.f90      
chimeredep2grib_exe_SOURCES = chimeredep2grib.f90   

chimerencdf2grib_exe_SOURCES = chimerencdf2grib.f90
chimerencdf2grib_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2011/libchimere2011.la
chimerencdfmet2grib_exe_SOURCES = chimerencdfmet2grib.f90
chimerencdfmet2grib_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2011/libchimere2011.la

chmbc2cdf_exe_SOURCES = chmbc2cdf.F90
chmbc2cdf_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2011/libchimere2011.la

column_content_exe_SOURCES = column_content.f90
crea_coord_chimere_exe_SOURCES = crea_coord_chimere.f90
exdom_grib_exe_SOURCES = exdom_grib.f90	      
filtra_spec_out_exe_SOURCES = filtra_spec_out.f90   
grib2chimerencdf_exe_SOURCES = grib2chimerencdf.f90
grib2chimere_exe_SOURCES = grib2chimere.f90      

grib2diagmetncdf_exe_SOURCES = grib2diagmetncdf.F90
grib2diagmetncdf_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2011/libchimere2011.la

lm2ncf_exe_SOURCES = lm2ncf.F90
lm2ncf_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2011/libchimere2011.la

merge_unf_chimere_exe_SOURCES = merge_unf_chimere.f90 
mix_ratio_exe_SOURCES = mix_ratio.f90
prepcosmomed_exe_SOURCES = chimere_params.F90 io.F90 calendar.F90 cosmo_option.F90 subs.F90 prepcosmomed.F90
pres_ecmwf_exe_SOURCES = pres_ecmwf.f90
scale_unf_chimere_exe_SOURCES = scale_unf_chimere.f90
sum_unf_chimere_exe_SOURCES = sum_unf_chimere.f90
show_unf_chimere_exe_SOURCES = show_unf_chimere.f90

riscrivi_meteonc_exe_SOURCES = riscrivi_meteonc.F90
riscrivi_meteonc_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2011/libchimere2011.la

split_output_exe_SOURCES = split_output.f90      
sum_pm_species_exe_SOURCES = sum_pm_species.f90    
tinterp_sim_exe_SOURCES = tinterp_sim.f90	      

# diagmet_LMSMR4_exe_SOURCES = ver2007/modules/chimere_params.LMSMR4.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.LMSMR4.F90 ver2007/diag/diagbio_common.LMSMR4.F90 ver2007/diag/diagmet_science.LMSMR4.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.LMSMR4.F90
# diagmet_LMSMR4_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2007/libchimere2007lmsmr4.la

# diagmet_LAMAZ_exe_SOURCES = ver2007/modules/chimere_params.LAMAZ.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.LAMAZ.F90 ver2007/diag/diagbio_common.LAMAZ.F90 ver2007/diag/diagmet_science.LAMAZ.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.LAMAZ.F90
# diagmet_LAMAZ_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2007/libchimere2007lamaz.la

# diagmet_LAMA3_exe_SOURCES = ver2007/modules/chimere_params.LAMA3.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.LAMA3.F90 ver2007/diag/diagbio_common.LAMA3.F90 ver2007/diag/diagmet_science.LAMA3.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.LAMA3.F90
# diagmet_LAMA3_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2007/libchimere2007lama3.la

diagmet_LAMA5_exe_SOURCES = ver2007/modules/chimere_params.LAMA5.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.LAMA5.F90 ver2007/diag/diagbio_common.LAMA5.F90 ver2007/diag/diagmet_science.LAMA5.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.LAMA5.F90
diagmet_LAMA5_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2007/libchimere2007lama5.la

diagmet_ITA5KM_exe_SOURCES = ver2007/modules/chimere_params.ITA5KM.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.ITA5KM.F90 ver2007/diag/diagbio_common.ITA5KM.F90 ver2007/diag/diagmet_science.ITA5KM.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.ITA5KM.F90
diagmet_ITA5KM_exe_LDADD = $(LDADD) $(top_builddir)/chimere/src/ver2007/libchimere2007ita5km.la


CLEANFILES = \
	diagmet_science.mod \
	chimere_params.mod \
	chimere_consts.mod \
	diagbio_common.mod \
	diagmet_common.mod
