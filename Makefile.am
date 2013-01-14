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
	post_lm/src

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

postlmpkgdatarootdir = $(pkgdatarootdir)/post_lm/dat
postlmpkgdataroot_DATA = \
	$(top_srcdir)/post_lm/dat/incen.ptn \
	$(top_srcdir)/post_lm/dat/incen.rot \
	$(top_srcdir)/post_lm/dat/incen.utm \
	$(top_srcdir)/post_lm/dat/inversione_35lay.filt \
	$(top_srcdir)/post_lm/dat/inversione_40lay.filt \
	$(top_srcdir)/post_lm/dat/lama_all.filt \
	$(top_srcdir)/post_lm/dat/lama_ctn.filt \
	$(top_srcdir)/post_lm/dat/lama_fop.filt \
	$(top_srcdir)/post_lm/dat/lama_missing_wind_component.filt \
	$(top_srcdir)/post_lm/dat/lama_sop.filt \
	$(top_srcdir)/post_lm/dat/lm_ninfa.filt \
	$(top_srcdir)/post_lm/dat/lsm_ecmwf.grb \
	$(top_srcdir)/post_lm/dat/lsm_lami.grb \
	$(top_srcdir)/post_lm/dat/post_lama.filt

postlmdiagmetpkgdatarootdir = $(pkgdatarootdir)/post_lm/dat/diagmet
postlmdiagmetpkgdataroot_DATA = \
	$(top_srcdir)/post_lm/dat/diagmet/COORD_LAMAZ \
	$(top_srcdir)/post_lm/dat/diagmet/COORD_LMSMR4 \
	$(top_srcdir)/post_lm/dat/diagmet/LAMAZ_lfrac.grb \
	$(top_srcdir)/post_lm/dat/diagmet/LANDPAR.inputdata.211011021.8 \
	$(top_srcdir)/post_lm/dat/diagmet/LANDUSE_LAMAZ \
	$(top_srcdir)/post_lm/dat/diagmet/LANDUSE_LAMAZ.2005 \
	$(top_srcdir)/post_lm/dat/diagmet/LANDUSE_LAMAZ.2011 \
	$(top_srcdir)/post_lm/dat/diagmet/LANDUSE_LMSMR4 \
	$(top_srcdir)/post_lm/dat/diagmet/LMSMR4_lfrac_2012.grb \
	$(top_srcdir)/post_lm/dat/diagmet/LMSMR4_lfrac.grb \
	$(top_srcdir)/post_lm/dat/diagmet/METINFOS-cosmo.nml.sed \
	$(top_srcdir)/post_lm/dat/diagmet/VCOORD_8_995_500

EXTRA_DIST = \
	$(arkimetpkgdataroot_DATA) \
	$(osservazionipkgdataroot_DATA) \
	$(postlmpkgdataroot_DATA) \
	$(postlmdiagmetpkgdataroot_DATA)