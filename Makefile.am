ACLOCAL_AMFLAGS = -I m4

SUBDIRS = \
	modules \
	arkimet/src \
	calmet/src \
	chimere/src/ver2007 \
	chimere/src/ver2011 \
	chimere/src \
	fisiog/src \
	osservazioni/src \
	post_lm/src \
	util/grads/src \
	util/grib/src

pkgdatarootdir = $(datarootdir)/$(PACKAGE)
arkimetpkgdatarootdir = $(pkgdatarootdir)/arkimet/dat
arkimetpkgdataroot_DATA = \
	$(top_srcdir)/arkimet/dat/aree_geo.dat \
	$(top_srcdir)/arkimet/dat/aree_utm.dat \
	$(top_srcdir)/arkimet/dat/arkimet_aree.dat

utilgribpkgdatarootdir = $(pkgdatarootdir)/util/grib/dat
utilgribpkgdataroot_DATA = \
	$(top_srcdir)/util/grib/dat/tabella_002_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_002.txt \
	$(top_srcdir)/util/grib/dat/tabella_128_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_128.txt \
	$(top_srcdir)/util/grib/dat/tabella_195_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_195.txt \
	$(top_srcdir)/util/grib/dat/tabella_196_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_196.txt \
	$(top_srcdir)/util/grib/dat/tabella_197.txt \
	$(top_srcdir)/util/grib/dat/tabella_198.txt \
	$(top_srcdir)/util/grib/dat/tabella_199_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_199.txt \
	$(top_srcdir)/util/grib/dat/tabella_200_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_200.txt \
	$(top_srcdir)/util/grib/dat/tabella_201_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_201.txt \
	$(top_srcdir)/util/grib/dat/tabella_203_ser.txt \
	$(top_srcdir)/util/grib/dat/tabella_203.txt 

EXTRA_DIST = \
	$(arkimetpkgdataroot_DATA) \
	$(osservazionipkgdataroot_DATA) \
	$(postlmpkgdataroot_DATA) \
	$(postlmdiagmetpkgdataroot_DATA) \
	$(utilgribpkgdataroot_DATA)
