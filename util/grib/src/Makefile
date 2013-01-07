#===============================================================================
# Makefile for dir: util/grib
#
# make all:                re-compile all updated programs
# make clean:              delete all executables
# make ../bin/program.exe: re-compile the specifed program (if needed)
#
# Programs presently not compiling:
# ../bin/calc_ipgt_inv_multi.exe
# ../bin/decumula_lm.exe
#
# Notes: 
# - The "laboratory programs" (templates that must be modified for every 
#   application) are not included in this Makefile.
# - To add a new program: add the program name to PROGRAMS varible, then add a 
#   rule (see template rule)
#
# Todo:
# - Implement compilation with debug options without modifying Makefile
#   (ie. make mode=debug)
# - Implement MA modules without explicitly adding them to the list of source
#   files (as it already happens for stantard libsim modules) 
#
#===============================================================================

# Assignments that can be changed
PROGRAMS = ../bin/afa2grib.exe ../bin/crea_templ_grib2_utm.exe ../bin/cong2.exe ../bin/diff_grib.exe ../bin/diff_gribex.exe ../bin/diff_header_gribex.exe ../bin/grib2afa.exe ../bin/grib2latlon.exe ../bin/grib_ana2forc.exe ../bin/grib_ave.exe ../bin/grib_calc_median.exe ../bin/grib_daily_stat.exe ../bin/grib_dist_freq.exe ../bin/grib_f2s.exe ../bin/grib_forc2ana.exe ../bin/grib_local_std.exe ../bin/grib_runmean.exe ../bin/grib_s2f.exe ../bin/grib_skip_first.exe ../bin/grib_stat.exe ../bin/grib_step_explicit.exe ../bin/grib_uv2ffdd.exe ../bin/intercala_grib.exe ../bin/interp_grib.exe ../bin/llgrib.exe ../bin/llgribex.exe ../bin/ma_grib1_grib2.exe ../bin/ma_grib2_grib1.exe ../bin/mask_grib.exe ../bin/math_grib.exe ../bin/moltiplica_grib.exe ../bin/proc_flat_grib.exe ../bin/replace_dat_grib.exe ../bin/somma_grib.exe ../bin/split_grib_par.exe ../bin/split_gribex_par.exe ../bin/test_1grib.exe ../bin/tinterp_grib.exe ../bin/uniq_grib.exe ../bin/zoom_grib.exe

#FCFLAGS = -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check -g
FCFLAGS =

# Fixed assignments
FC = gfortran
#INCFLAGS = -I/usr/include -I/usr/lib/gfortran/modules
INCFLAGS = -I/usr/include -I/usr/lib64/gfortran/modules
LIBGAPI = -lgrib_api -lgrib_api_f90
LIBGBEX = -lemos
LIBLSIM = -lsim_base
LIBLSIMVG6D = -lsim_volgrid6d
LIBLSIMV7D = -lsim_vol7d
MODDIR = ${CURDIR}/../../../modules
MODSERIET = ${MODDIR}/seriet_utilities.f90
MODGRIB2 = ${MODDIR}/grib2_utilities.f90
MODDATE = ${MODDIR}/date_handler.f90
MODGRID = ${MODDIR}/grid_handler.f90

# Path of metamb modules
VPATH = ${MODDIR}

# Utility targets
all: $(PROGRAMS)

.PHONY: clean
clean: 
	$(RM) $(PROGRAMS) *.mod *.o

# Template rule
../bin/tmp.exe: tmp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

# Rules to compile actual programs

../bin/afa2grib.exe: afa2grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/crea_templ_grib2_utm.exe: crea_templ_grib2_utm.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/cong2.exe: cong2.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/diff_grib.exe: diff_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI)

../bin/diff_gribex.exe: diff_gribex.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/diff_header_gribex.exe: diff_header_gribex.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib2afa.exe: grib2afa.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/grib2latlon.exe: grib2latlon.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_ana2forc.exe: grib_ana2forc.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/grib_ave.exe: grib_ave.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_calc_median.exe: grib_calc_median.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/grib_daily_stat.exe: grib_daily_stat.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/grib_dist_freq.exe: grib_dist_freq.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/grib_f2s.exe: grib_f2s.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_forc2ana.exe: grib_forc2ana.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/grib_local_std.exe: grib_local_std.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_runmean.exe: grib_runmean.f90 date_handler.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/grib_s2f.exe: grib_s2f.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_skip_first.exe: grib_skip_first.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/grib_stat.exe: grib_stat.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_step_explicit.exe: grib_step_explicit.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/grib_uv2ffdd.exe: grib_uv2ffdd.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/intercala_grib.exe: intercala_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/interp_grib.exe: interp_grib.f90 grid_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRID) $< $(LIBGBEX)

../bin/llgrib.exe: llgrib.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/llgribex.exe: llgribex.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/ma_grib1_grib2.exe: ma_grib1_grib2.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI)

../bin/ma_grib2_grib1.exe: ma_grib2_grib1.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

../bin/mask_grib.exe: mask_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/math_grib.exe: math_grib.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/moltiplica_grib.exe: moltiplica_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/proc_flat_grib.exe: proc_flat_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/replace_dat_grib.exe: replace_dat_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/somma_grib.exe: somma_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/split_grib_par.exe: split_grib_par.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/split_gribex_par.exe: split_gribex_par.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/test_1grib.exe: test_1grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/tinterp_grib.exe: tinterp_grib.f90 grib2_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/uniq_grib.exe: uniq_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/zoom_grib.exe: zoom_grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)



