#===============================================================================
# Makefile for dir: fisiog
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
PROGRAMS = ../bin/proc_fisiog.exe ../bin/calc_tpi.exe

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

CHI2007DIR = 
CHI2011DIR = ${CURDIR}/../../chimere/src/ver2011
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
	rm -f $(PROGRAMS) *.mod *.o

# Template rule
../bin/tmp.exe: tmp.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI) $(LIBGBEX)

# Rules to compile required programs
../bin/proc_fisiog.exe: proc_fisiog.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/calc_tpi.exe: calc_tpi.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)
