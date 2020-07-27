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
 $(top_srcdir)/data/anag_stzqa.dat \
 $(top_srcdir)/data/anag_synop.csv \
 $(top_srcdir)/data/anag_temp.csv \
 $(top_srcdir)/data/aree_geo.dat \
 $(top_srcdir)/data/aree_utm.dat \
 $(top_srcdir)/data/arkimet_aree.dat \
 $(top_srcdir)/data/COS2I_orog_20180825.grb \
 $(top_srcdir)/data/COS2I_orog_20180825.ctl \
 $(top_srcdir)/data/COS2I_orog_20180825.idx \
 $(top_srcdir)/data/COS5MITA_orog_20170405.grb \
 $(top_srcdir)/data/COS5MITA_orog_20170405.ctl \
 $(top_srcdir)/data/COS5MITA_orog_20170405.idx \
 $(top_srcdir)/data/COS5MITR_layers_20180514.grb \
 $(top_srcdir)/data/COS5MITR_layersSup_20180514.grb \
 $(top_srcdir)/data/COS5MITR_orog_20180514.grb \
 $(top_srcdir)/data/COS5MITR_orog_20180514.ctl \
 $(top_srcdir)/data/COS5MITR_orog_20180514.idx \
 $(top_srcdir)/data/COS5MMEDL_orog_20170405.grb \
 $(top_srcdir)/data/COS5MMEDL_orog_20170405.ctl \
 $(top_srcdir)/data/COS5MMEDL_orog_20170405.idx \
 $(top_srcdir)/data/eur20k_orog.ctl \
 $(top_srcdir)/data/eur20k_orog.grb \
 $(top_srcdir)/data/eur20k_orog.idx \
 $(top_srcdir)/data/gacsv2seriet.nml.def \
 $(top_srcdir)/data/gacsv2seriet.nml.adms \
 $(top_srcdir)/data/LAMA5_layers_20180514.grb \
 $(top_srcdir)/data/LAMA5_layersSup_20180514.grb \
 $(top_srcdir)/data/LAMA5_orog_20180514.grb \
 $(top_srcdir)/data/LAMA5_orog_20180514.ctl \
 $(top_srcdir)/data/LAMA5_orog_20180514.idx \
 $(top_srcdir)/data/LAMAZ_layers_20120606.grb \
 $(top_srcdir)/data/LAMAZ_layersSup_20120606.grb \
 $(top_srcdir)/data/LAMAZ_orog_20120606.grb \
 $(top_srcdir)/data/LAMAZ_orog_20120606.ctl \
 $(top_srcdir)/data/LAMAZ_orog_20120606.idx \
 $(top_srcdir)/data/marks_emr.dat \
 $(top_srcdir)/data/marks_ita_rot1.dat \
 $(top_srcdir)/data/marks_ita_rot2.dat \
 $(top_srcdir)/data/marks_ita_rot_c1043.dat \
 $(top_srcdir)/data/marks_ita_rot_c1057.dat \
 $(top_srcdir)/data/marks_nord_rot1.dat \
 $(top_srcdir)/data/marks_nord_rot2.dat \
 $(top_srcdir)/data/marks_nord_rot_c1043.dat \
 $(top_srcdir)/data/marks_nord_rot_c1057.dat \
 $(top_srcdir)/data/match-alias.conf.arkimet \
 $(top_srcdir)/data/nita1k_orog.ctl \
 $(top_srcdir)/data/nita1k_orog.grb \
 $(top_srcdir)/data/nita1k_orog.idx \
 $(top_srcdir)/data/nita5kutm_orog.ctl \
 $(top_srcdir)/data/nita5kutm_orog.grb \
 $(top_srcdir)/data/nita5kutm_orog.idx \
 $(top_srcdir)/data/param_arkioss.csv \
 $(top_srcdir)/data/param_shortnames.csv \
 $(top_srcdir)/data/shape_emr.dat \
 $(top_srcdir)/data/shape_eur.dat \
 $(top_srcdir)/data/shape_regit.dat \
 $(top_srcdir)/data/shape_regitrot1.dat \
 $(top_srcdir)/data/shape_regitrot2.dat \
 $(top_srcdir)/data/shape_regitrot_c1043.dat \
 $(top_srcdir)/data/shape_regitrot_c1057.dat \
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
 $(top_srcdir)/data/template.akq.cosmo \
 $(top_srcdir)/data/template.akq.cosmo.vol \
 $(top_srcdir)/data/template.akq.def \
 $(top_srcdir)/data/template.akq.kenda \
 $(top_srcdir)/data/template.akq.lama5 \
 $(top_srcdir)/data/template.akq.lama5.adms \
 $(top_srcdir)/data/template.akq.lama5.calpuff \
 $(top_srcdir)/data/template.akq.lamaz \
 $(top_srcdir)/data/template.akq.lamaz.adms \
 $(top_srcdir)/data/template.akq.lamaz.calpuff \
 $(top_srcdir)/data/template.akq.ninfa \
 $(top_srcdir)/data/template.akq.ninfa.ub

if SMND_BUILD
else
pkgdataroot_DATA = $(datafilelist)
endif

EXTRA_DIST = $(datafilelist) ma_utils.spec
