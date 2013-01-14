ACLOCAL_AMFLAGS = -I m4

SUBDIRS = \
	modules \
	arkimet/src \
	calmet/src \
	chimere/src/ver2007 \
	chimere/src/ver2011 \
	chimere/src

#	chimere/src/ver2011/
#	chimere/src/ver2007/tools


#SUBDIRS = arkimet/src calmet/src chimere/src fisiog/src osservazioni/src post_lm/src util/grads/src util/grib/src

pkgdatarootdir = $(datarootdir)/$(PACKAGE)
arkimetpkgdatarootdir = $(pkgdatarootdir)/arkimet/dat
arkimetpkgdataroot_DATA = \
	$(top_srcdir)/arkimet/dat/aree_geo.dat \
	$(top_srcdir)/arkimet/dat/aree_utm.dat \
	$(top_srcdir)/arkimet/dat/arkimet_aree.dat

EXTRA_DIST = $(arkimetpkgdataroot_DATA)
