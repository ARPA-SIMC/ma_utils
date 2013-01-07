#===============================================================================
# Makefile for dir: post_lm
#
# make all:                re-compile all updated programs
# make clean:              delete all executables
# make ../bin/program.exe: re-compile the specifed program (if needed)
#
# Programs presently not compiling:
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
PROGRAMS = ../bin/calc_anh01.exe ../bin/calc_inv_max.exe ../bin/calc_ipgt.exe ../bin/calc_ipgt_inv_multi.exe ../bin/calc_ps_cosmo.exe ../bin/calc_swdown.exe ../bin/calc_ustar_mo.exe ../bin/correct_lamaz.exe ../bin/decumula_lm.exe ../bin/grib_ttd2rh.exe ../bin/lm_levels2layers.exe ../bin/post_wind_lm.exe ../bin/post_zi_diagmet.exe ../bin/qsoil_ecmwf2lama.exe ../bin/split_qrs.exe ../bin/stat_qrs_temp.exe 

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
MODDIR = ${CURDIR}/../../modules
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
	rm -f $(PROGRAMS) *.mod *.o

# Template rule
../bin/tmp.exe: tmp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

# Rules to compile actual programs

../bin/calc_anh01.exe: calc_anh01.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGAPI)

../bin/calc_inv_max.exe: calc_inv_max.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/calc_ipgt.exe: calc_ipgt.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/calc_ipgt_inv_multi.exe: calc_ipgt_inv_multi.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/calc_ps_cosmo.exe: calc_ps_cosmo.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRIB2) $< $(LIBLSIM) $(LIBGAPI)

../bin/calc_swdown.exe: calc_swdown.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI)

../bin/calc_ustar_mo.exe: calc_ustar_mo.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/correct_lamaz.exe: correct_lamaz.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX) $(LIBLSIM)

../bin/decumula_lm.exe: decumula_lm.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/grib_ttd2rh.exe: grib_ttd2rh.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/lm_levels2layers.exe: lm_levels2layers.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/post_wind_lm.exe: post_wind_lm.f90 grid_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODGRID) $< $(LIBGBEX)

../bin/post_zi_diagmet.exe: post_zi_diagmet.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/qsoil_ecmwf2lama.exe: qsoil_ecmwf2lama.f90 date_handler.f90 grid_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $(MODGRID) $< $(LIBGBEX)

../bin/split_qrs.exe: split_qrs.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/stat_qrs_temp.exe: stat_qrs_temp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

