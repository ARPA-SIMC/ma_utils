#===============================================================================
# Makefile for dir: osservazioni
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
PROGRAMS = ../bin/filtra_anag_dat.exe ../bin/maskout_orari.exe ../bin/orari_daily_stat.exe ../bin/orari2grads.exe ../bin/proc_multi_orari.exe ../bin/proc_sodar_moniter.exe ../bin/stat_orari.exe ../bin/trasp_temp.exe

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
	rm -f $(PROGRAMS) *.o *.mod

# Template rule
../bin/tmp.exe: tmp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

# Rules to compile actual programs

../bin/filtra_anag_dat.exe: filtra_anag_dat.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/maskout_orari.exe: maskout_orari.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/orari_daily_stat.exe: orari_daily_stat.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/orari2grads.exe: orari2grads.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/proc_multi_orari.exe: proc_multi_orari.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBLSIM)

../bin/proc_sodar_moniter.exe: proc_sodar_moniter.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM)

../bin/stat_orari.exe: stat_orari.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBLSIM)

../bin/trasp_temp.exe: trasp_temp.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<


