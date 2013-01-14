ACLOCAL_AMFLAGS = -I m4

SUBDIRS = \
	modules \
	arkimet/src \
	calmet/src \
	chimere/src/ver2007 \
	chimere/src/ver2011 \
	chimere/src \
	fisiog/src \
	osservazioni/src

#SUBDIRS = arkimet/src calmet/src chimere/src fisiog/src osservazioni/src post_lm/src util/grads/src util/grib/src

pkgdatarootdir = $(datarootdir)/$(PACKAGE)
arkimetpkgdatarootdir = $(pkgdatarootdir)/arkimet/dat
arkimetpkgdataroot_DATA = \
	$(top_srcdir)/arkimet/dat/aree_geo.dat \
	$(top_srcdir)/arkimet/dat/aree_utm.dat \
	$(top_srcdir)/arkimet/dat/arkimet_aree.dat

osservazionipkgdatarootdir = $(pkgdatarootdir)/osservazioni/dat
osservazionipkgdataroot_DATA = \
	$(top_srcdir)/osservazioni/dat/db_anagrafica.dat \
	$(top_srcdir)/osservazioni/dat/db_param.dat \
	$(top_srcdir)/osservazioni/dat/db_reti.dat

EXTRA_DIST = \
	$(arkimetpkgdataroot_DATA) \
	$(osservazionipkgdataroot_DATA)
