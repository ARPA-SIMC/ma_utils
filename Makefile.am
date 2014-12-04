ACLOCAL_AMFLAGS = -I m4

SUBDIRS = \
	modules \
	arkimet/src \
	arkimet/sh \
	arkimet/postprocessors/seriet \
	calmet/src \
	calmet/sh \
	chimere/src/ver2007 \
	chimere/src/ver2011 \
	chimere/src \
	chimere/sh \
	fisiog/src \
	osservazioni/src \
	post_lm/src \
	util/grads/src \
	util/grib/src

pkgdatarootdir = $(datarootdir)/$(PACKAGE)

pkgdataroot_DATA = \
	$(top_srcdir)/data/anag_arkioss.csv \
	$(top_srcdir)/data/anag_oracle.dat \
	$(top_srcdir)/data/anag_synop.csv \
	$(top_srcdir)/data/anag_temp.csv \
	$(top_srcdir)/data/aree_geo.dat \
	$(top_srcdir)/data/aree_utm.dat \
	$(top_srcdir)/data/arkimet_aree.dat \
	$(top_srcdir)/data/param_arkioss.csv \
	$(top_srcdir)/data/param_shortnames.csv \
	$(top_srcdir)/data/tabella_002_ser.txt \
	$(top_srcdir)/data/tabella_002.txt \
	$(top_srcdir)/data/tabella_128_ser.txt \
	$(top_srcdir)/data/tabella_128.txt \
	$(top_srcdir)/data/tabella_195_ser.txt \
	$(top_srcdir)/data/tabella_195.txt \
	$(top_srcdir)/data/tabella_196_ser.txt \
	$(top_srcdir)/data/tabella_196.txt \
	$(top_srcdir)/data/tabella_199_ser.txt \
	$(top_srcdir)/data/tabella_199.txt \
	$(top_srcdir)/data/tabella_200_ser.txt \
	$(top_srcdir)/data/tabella_200.txt \
	$(top_srcdir)/data/tabella_201_ser.txt \
	$(top_srcdir)/data/tabella_201.txt \
	$(top_srcdir)/data/tabella_203_ser.txt \
	$(top_srcdir)/data/tabella_203.txt \
	$(top_srcdir)/data/LAMAZ_layers_20120606.grb \
	$(top_srcdir)/data/LAMAZ_orog_20120606.grb \
	$(top_srcdir)/data/LMSMR4_layers_20120606.grb \
	$(top_srcdir)/data/LMSMR4_orog_20120606.grb \
	$(top_srcdir)/data/LMSMR5_layers_20120606.grb \
	$(top_srcdir)/data/LMSMR5_orog_20120606.grb

EXTRA_DIST = $(pkgdataroot_DATA)
