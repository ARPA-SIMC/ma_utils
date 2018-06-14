ACLOCAL_AMFLAGS = -I m4

if SMND_BUILD
SUBDIRS = \
	modules \
	arkimet/src \
	arkimet/sh \
	arkimet/postprocessors/seriet \
	util/grib/src
else
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
	osservazioni/sh \
	post_lm/src \
	util/grads/src \
	util/grib/src
endif

pkgdatarootdir = $(datarootdir)/$(PACKAGE)

datafilelist = \
 $(top_srcdir)/data/anag_arkioss.csv \
 $(top_srcdir)/data/anag_oracle.dat \
 $(top_srcdir)/data/anag_synop.csv \
 $(top_srcdir)/data/anag_temp.csv \
 $(top_srcdir)/data/aree_geo.dat \
 $(top_srcdir)/data/aree_utm.dat \
 $(top_srcdir)/data/arkimet_aree.dat \
 $(top_srcdir)/data/gacsv2seriet.nml.lama.adms \
 $(top_srcdir)/data/param_arkioss.csv \
 $(top_srcdir)/data/param_shortnames.csv \
 $(top_srcdir)/data/tabella_002_ser.txt \
 $(top_srcdir)/data/tabella_002.txt \
 $(top_srcdir)/data/tabella_128_ser.txt \
 $(top_srcdir)/data/tabella_128.txt \
 $(top_srcdir)/data/tabella_194_ser.txt \
 $(top_srcdir)/data/tabella_194.txt \
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
 $(top_srcdir)/data/template.akq.cosmo_5M_vol_ita \
 $(top_srcdir)/data/template.akq.lama5 \
 $(top_srcdir)/data/template.akq.lamaz \
 $(top_srcdir)/data/template.akq.lama.adms \
 $(top_srcdir)/data/template.akq.lama.calpuff \
 $(top_srcdir)/data/template.akq.ninfa5ub \
 $(top_srcdir)/data/template.akq.ninfabpa5 \
 $(top_srcdir)/data/template.akq.pescoemrope \
 $(top_srcdir)/data/match-alias.conf.arkimet \
 $(top_srcdir)/data/LAMAZ_layers_20120606.grb \
 $(top_srcdir)/data/LAMAZ_layersSup_20120606.grb \
 $(top_srcdir)/data/LAMAZ_orog_20120606.grb \
 $(top_srcdir)/data/LAMAZ_orog_20120606.ctl \
 $(top_srcdir)/data/LAMAZ_orog_20120606.idx \
 $(top_srcdir)/data/LAMAZ_layers.grb \
 $(top_srcdir)/data/LAMAZ_layersSup.grb \
 $(top_srcdir)/data/LAMAZ_orog.grb \
 $(top_srcdir)/data/COS5MITR_layers_20180514.grb \
 $(top_srcdir)/data/COS5MITR_layersSup_20180514.grb \
 $(top_srcdir)/data/COS5MITR_orog_20180514.grb \
 $(top_srcdir)/data/COS5MITR_orog_20180514.ctl \
 $(top_srcdir)/data/COS5MITR_orog_20180514.idx \
 $(top_srcdir)/data/COS5MITR_layers.grb \
 $(top_srcdir)/data/COS5MITR_layersSup.grb \
 $(top_srcdir)/data/COS5MITR_orog.grb \
 $(top_srcdir)/data/LAMA5_layers_20180514.grb \
 $(top_srcdir)/data/LAMA5_layersSup_20180514.grb \
 $(top_srcdir)/data/LAMA5_orog_20180514.grb \
 $(top_srcdir)/data/LAMA5_orog_20180514.ctl \
 $(top_srcdir)/data/LAMA5_orog_20180514.idx \
 $(top_srcdir)/data/LAMA5_layers.grb \
 $(top_srcdir)/data/LAMA5_layersSup.grb \
 $(top_srcdir)/data/LAMA5_orog.grb

if SMND_BUILD
else
pkgdataroot_DATA = $(datafilelist)
endif

EXTRA_DIST = $(datafilelist) ma_utils.spec
