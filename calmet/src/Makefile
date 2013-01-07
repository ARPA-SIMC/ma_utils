#===============================================================================
# Makefile for dir: calmet
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
PROGRAMS = ../bin/append_calmet_dat.exe ../bin/asc2calmet_dat.exe ../bin/calc_staz_dist.exe ../bin/calmet2grib.exe ../bin/calmet_dat2asc.exe ../bin/crea_calmet_inp.exe ../bin/crea_date_calmet.exe ../bin/crea_prq_surf.exe  ../bin/crea_wt_dat.exe ../bin/crea_surf_dat.exe ../bin/crea_surf_req.exe ../bin/dummy_grib_calmet.exe ../bin/extrap_wind.exe ../bin/geo_dat2grib.exe ../bin/grib23ddat.exe ../bin/grib2mm5.exe ../bin/grib2up_dat.exe ../bin/proc_seriet_prf.exe ../bin/proc_seriet_surf.exe ../bin/proc_st1a.exe ../bin/rw_dat_calmet.exe ../bin/rw_xout_calmet.exe ../bin/sel_staz_calmet.exe ../bin/test_inp_calmet.exe ../bin/xout2grib.exe

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

../bin/append_calmet_dat.exe: append_calmet_dat.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/asc2calmet_dat.exe: asc2calmet_dat.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/calc_staz_dist.exe: calc_staz_dist.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/calmet2grib.exe: calmet2grib.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/calmet_dat2asc.exe: calmet_dat2asc.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/crea_calmet_inp.exe: crea_calmet_inp.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/crea_date_calmet.exe: crea_date_calmet.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/crea_prq_surf.exe: crea_prq_surf.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/crea_wt_dat.exe: crea_wt_dat.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/crea_surf_dat.exe: crea_surf_dat.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/crea_surf_req.exe: crea_surf_req.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/dummy_grib_calmet.exe: dummy_grib_calmet.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBGBEX)

../bin/extrap_wind.exe: extrap_wind.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/geo_dat2grib.exe: geo_dat2grib.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $< $(LIBLSIM) $(LIBGAPI)

../bin/grib23ddat.exe: grib23ddat.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGAPI)

../bin/grib2mm5.exe: grib2mm5.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

../bin/grib2up_dat.exe: grib2up_dat.f90 date_handler.f90 seriet_utilities.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $(MODSERIET) $< $(LIBLSIM) $(LIBGBEX)

../bin/proc_seriet_prf.exe: proc_seriet_prf.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/proc_seriet_surf.exe: proc_seriet_surf.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/proc_st1a.exe: proc_st1a.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/rw_dat_calmet.exe: rw_dat_calmet.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/rw_xout_calmet.exe: rw_xout_calmet.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/sel_staz_calmet.exe: sel_staz_calmet.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $<

../bin/test_inp_calmet.exe: test_inp_calmet.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $<

../bin/xout2grib.exe: xout2grib.f90 date_handler.f90
	$(FC) $(FCFLAGS) -o $@ $(INCFLAGS) $(MODDATE) $< $(LIBGBEX)

