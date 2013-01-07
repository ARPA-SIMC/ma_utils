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
PROGRAMS = ../bin/altipres_sim.exe ../bin/chimere2grib.exe ../bin/chimeredep2grib.exe ../bin/chimerencdf2grib.exe ../bin/chmbc2cdf.exe ../bin/crea_coord_chimere.exe ../bin/exdom_grib.exe ../bin/filtra_spec_out.exe ../bin/grb_chimere2netcdf.exe ../bin/grib2chimere.exe ../bin/lm2ncf.exe ../bin/merge_unf_chimere.exe  ../bin/mix_ratio.exe ../bin/pres_ecmwf.exe ../bin/scale_unf_chimere.exe ../bin/sum_unf_chimere.exe ../bin/show_unf_chimere.exe ../bin/riscrivi_meteonc.exe ../bin/split_output.exe ../bin/sum_pm_species.exe ../bin/tinterp_sim.exe ../bin/diagmet_LMSMR4.exe ../bin/diagmet_LAMAZ.exe

#FCFLAGS = -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check -g
FCFLAGS =

# Fixed assignments
FC = gfortran
#INCFLAGS = -I/usr/include -I/usr/lib/gfortran/modules
INCFLAGS = -I/usr/include -I/usr/lib64/gfortran/modules
LIBGAPI = -lgrib_api -lgrib_api_f90
LIBGBEX = -lemos
LIBGUTIL = -lgrib_util
LIBNETCDF = -lnetcdff
LIBLSIM = -lsim_base
LIBLSIMVG6D = -lsim_volgrid6d
LIBLSIMV7D = -lsim_vol7d

# Path of "official" Chimere utilities
CHI2007DIR = ${CURDIR}/ver2007
CHI2011DIR = ${CURDIR}/ver2011
CHIUTIL2007 = ${CHI2007DIR}/tools/calendar.F90 ${CHI2007DIR}/tools/subs.F90 ${CHI2007DIR}/tools/io.F90 ${CHI2007DIR}/tools/layave.F90
CHIUTIL2011 = ${CHI2011DIR}/tools/calendar.F90 ${CHI2011DIR}/tools/subs.F90 ${CHI2011DIR}/tools/io.F90 ${CHI2011DIR}/tools/layave.F90

# Path of metamb modules
MODDIR = ${CURDIR}/../../modules
MODSERIET = ${MODDIR}/seriet_utilities.f90
MODGRIB2 = ${MODDIR}/grib2_utilities.f90
MODDATE = ${MODDIR}/date_handler.f90
MODGRID = ${MODDIR}/grid_handler.f90
VPATH = ${MODDIR}

# Utility targets
all: $(PROGRAMS)

.PHONY: clean
clean: 
	$(RM) $(PROGRAMS) *.mod *.o

# Template rule
../bin/tmp.exe: tmp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

# Rule for "COSMO-OPE" versions of diagmet (used for operational COSMO post-processing)
# source file chimere_params.F90 depends on domain shape!
../bin/diagmet_LMSMR4.exe: ver2007/modules/chimere_params.LMSMR4.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.F90 ver2007/diag/diagbio_common.F90 ver2007/diag/diagmet_science.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.F90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $^ $(CHIUTIL2007) -ffree-form $(LIBNETCDF) 

../bin/diagmet_LAMAZ.exe: ver2007/modules/chimere_params.LAMAZ.F90 ver2007/modules/chimere_consts.F90 ver2007/diag/diagmet_common.F90 ver2007/diag/diagbio_common.F90 ver2007/diag/diagmet_science.F90 ver2007/diag/deepconvtiedke.F90 ver2007/diag/diagmet.F90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $^ $(CHIUTIL2007) -ffree-form $(LIBNETCDF) 

# Rules to compile SIMC programs
../bin/altipres_sim.exe: altipres_sim.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/chimere2grib.exe: chimere2grib.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/chimeredep2grib.exe: chimeredep2grib.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/chimerencdf2grib.exe: chimerencdf2grib.F90 $(CHIUTIL2011) date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(CHIUTIL2011) $(MODDATE) $< $(LIBNETCDF) $(LIBGBEX)

../bin/chmbc2cdf.exe: chmbc2cdf.F90 $(CHIUTIL2011)
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(CHIUTIL2011) $< $(LIBNETCDF)

../bin/crea_coord_chimere.exe: crea_coord_chimere.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/exdom_grib.exe: exdom_grib.f90 date_handler.f90 grid_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $(MODGRID) $< $(LIBGBEX)

../bin/filtra_spec_out.exe: filtra_spec_out.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/grb_chimere2netcdf.exe: grb_chimere2netcdf.f90 date_handler.f90 
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX) $(LIBNETCDF)

../bin/grib2chimere.exe: grib2chimere.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/lm2ncf.exe: lm2ncf.F90 $(CHIUTIL2011)
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(CHIUTIL2011) $< $(LIBNETCDF)

../bin/merge_unf_chimere.exe: merge_unf_chimere.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/mix_ratio.exe: mix_ratio.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/riscrivi_meteonc.exe: riscrivi_meteonc.F90 $(CHIUTIL2011) date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(CHIUTIL2011) $(MODDATE) $< $(LIBNETCDF)

../bin/pres_ecmwf.exe: pres_ecmwf.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/scale_unf_chimere.exe: scale_unf_chimere.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/sum_unf_chimere.exe: sum_unf_chimere.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/show_unf_chimere.exe: show_unf_chimere.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/split_output.exe: split_output.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/sum_pm_species.exe: sum_pm_species.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/tinterp_sim.exe: tinterp_sim.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<





