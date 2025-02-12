AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I$(top_builddir)/modules -DPKGDATAROOTDIR='"$(datarootdir)/$(PACKAGE)"' -cpp
LDADD = $(top_builddir)/modules/libmautils.la

if SMND_BUILD
pkglibexec_PROGRAMS = grib_runmean.exe grib_skip_first.exe grib_smooth.exe math_grib.exe split_grib_par.exe ma_grib1_grib2.exe ma_grib2_grib1.exe grib2_forc2ana.exe
else
pkglibexec_PROGRAMS = crea_templ_grib2_utm.exe diff_grib.exe grib2afa.exe grib_ana2forc.exe grib_calc_median.exe grib_daily_stat.exe grib_forc2ana.exe grib2_forc2ana.exe grib_rms_diff.exe grib_runmean.exe grib_set_scanning.exe grib_smooth.exe grib_skip_first.exe grib_stat.exe grib_uniq.exe grib_uv2ffdd.exe intercala_grib.exe llgrib.exe ma_grib1_grib2.exe ma_grib2_grib1.exe maskout_grib.exe math_grib.exe proc_flat_grib.exe split_grib_par.exe test_1grib.exe tinterp_grib.exe
endif

## Programs that call emoslib: diff_header_gribex.exe grib_dist_freq.exe moltiplica_grib.exe somma_grib.exe interp_grib.exe grib_step_explicit.exe grib_f2s.exe grib_s2f.exe diff_gribex.exe replace_dat_grib.exe ma_grib2_grib1_gribex.exe llgribex.exe split_gribex_par.exe grib_ave.exe grib2latlon.exe afa2grib.exe zoom_grib.exe mask_grib.exe grib_local_std.exe cong2.exe

afa2grib_exe_SOURCES = afa2grib.f90
crea_templ_grib2_utm_exe_SOURCES = crea_templ_grib2_utm.f90
cong2_exe_SOURCES = cong2.f90
diff_grib_exe_SOURCES = diff_grib.f90
diff_gribex_exe_SOURCES = diff_gribex.f90
diff_header_gribex_exe_SOURCES = diff_header_gribex.f90
grib2afa_exe_SOURCES = grib2afa.f90
grib2latlon_exe_SOURCES = grib2latlon.f90
grib_ana2forc_exe_SOURCES = grib_ana2forc.f90
grib_ave_exe_SOURCES = grib_ave.f90
grib_calc_median_exe_SOURCES = grib_calc_median.f90
grib_daily_stat_exe_SOURCES = grib_daily_stat.f90
grib_dist_freq_exe_SOURCES = grib_dist_freq.f90
grib_f2s_exe_SOURCES = grib_f2s.f90
grib_set_scanning_exe_SOURCES = grib_set_scanning.f90
grib_forc2ana_exe_SOURCES = grib_forc2ana.f90
grib2_forc2ana_exe_SOURCES = grib2_forc2ana.f90
grib_local_std_exe_SOURCES = grib_local_std.f90
grib_rms_diff_exe_SOURCES = grib_rms_diff.f90
grib_runmean_exe_SOURCES = grib_runmean.f90
grib_s2f_exe_SOURCES = grib_s2f.f90
grib_skip_first_exe_SOURCES = grib_skip_first.f90
grib_smooth_exe_SOURCES = grib_smooth.f90
grib_stat_exe_SOURCES = grib_stat.f90
grib_step_explicit_exe_SOURCES = grib_step_explicit.f90
grib_uniq_exe_SOURCES = grib_uniq.f90
grib_uv2ffdd_exe_SOURCES = grib_uv2ffdd.f90      
intercala_grib_exe_SOURCES = intercala_grib.f90    
interp_grib_exe_SOURCES = interp_grib.f90	      
llgrib_exe_SOURCES = llgrib.f90	      
llgribex_exe_SOURCES = llgribex.f90	      
ma_grib1_grib2_exe_SOURCES = ma_grib1_grib2.f90    
ma_grib2_grib1_exe_SOURCES = ma_grib2_grib1.f90    
ma_grib2_grib1_gribex_exe_SOURCES = ma_grib2_grib1_gribex.f90    
mask_grib_exe_SOURCES = mask_grib.f90	      
maskout_grib_exe_SOURCES = maskout_grib.f90	      
math_grib_exe_SOURCES = math_grib.f90	      
moltiplica_grib_exe_SOURCES = moltiplica_grib.f90   
proc_flat_grib_exe_SOURCES = proc_flat_grib.f90    
replace_dat_grib_exe_SOURCES = replace_dat_grib.f90  
somma_grib_exe_SOURCES = somma_grib.f90	      
split_grib_par_exe_SOURCES = split_grib_par.f90    
split_gribex_par_exe_SOURCES = split_gribex_par.f90  
test_1grib_exe_SOURCES = test_1grib.f90	      
tinterp_grib_exe_SOURCES = tinterp_grib.f90      
zoom_grib_exe_SOURCES = zoom_grib.f90         
