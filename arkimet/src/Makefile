#===============================================================================
# Makefile for dir: arkimet
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
PROGRAMS = ../bin/gacsv2seriet.exe ../bin/gacsv_scan.exe ../bin/indice2coor.exe ../bin/mvl_create.exe ../bin/pm_profile_zi.exe ../bin/ptn2pts_csv.exe ../bin/scores_seriet.exe ../bin/scores_seriet_wind.exe ../bin/sel_punti.exe ../bin/sel_punti_grid.exe ../bin/write_fisiog_csv.exe ../bin/write_libsim_anag.exe

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
	$(RM) $(PROGRAMS) *.mod *.o

# Template rule
../bin/tmp.exe: tmp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

# Rules to compile actual programs
../bin/gacsv2seriet.exe: gacsv2seriet.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM)

../bin/gacsv_scan.exe: gacsv_scan.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM)

../bin/indice2coor.exe: indice2coor.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/mvl_create.exe: mvl_create.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/pm_profile_zi.exe: pm_profile_zi.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/ptn2pts_csv.exe: ptn2pts_csv.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM)

../bin/scores_seriet.exe: scores_seriet.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/scores_seriet_wind.exe: scores_seriet_wind.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/sel_punti.exe: sel_punti.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM)

../bin/sel_punti_grid.exe: sel_punti_grid.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM)

../bin/write_fisiog_csv.exe: write_fisiog_csv.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM) $(LIBLSIMV7D)

../bin/write_libsim_anag.exe: write_libsim_anag.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODSERIET) $< $(LIBLSIM) $(LIBLSIMV7D)
