AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I/usr/lib/gfortran/modules

noinst_LTLIBRARIES = libchimere2007lama5.la libchimere2007ita5km.la

libchimere2007lama5_la_SOURCES = \
	modules/chimere_params.LAMA5.F90 \
	modules/chimere_consts.F90 \
	diag/diagmet_common.LAMA5.F90 \
	diag/diagbio_common.LAMA5.F90 \
	diag/diagmet_science.LAMA5.F90 \
	diag/deepconvtiedke.F90 \
	diag/diagmet.LAMA5.F90 \
	tools/calendar.F90 \
	tools/subs.F90 \
	tools/io.F90 \
	tools/layave.F90

libchimere2007ita5km_la_SOURCES = \
	modules/chimere_params.ITA5KM.F90 \
	modules/chimere_consts.F90 \
	diag/diagmet_common.ITA5KM.F90 \
	diag/diagbio_common.ITA5KM.F90 \
	diag/diagmet_science.ITA5KM.F90 \
	diag/deepconvtiedke.F90 \
	diag/diagmet.ITA5KM.F90 \
	tools/calendar.F90 \
	tools/subs.F90 \
	tools/io.F90 \
	tools/layave.F90

CLEANFILES = \
	diagmet_science.mod \
	chimere_params.mod \
	chimere_consts.mod \
	diagbio_common.mod \
	diagmet_common.mod
