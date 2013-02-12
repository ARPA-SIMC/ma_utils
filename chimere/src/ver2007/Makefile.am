AM_FCFLAGS = -I/usr/lib64/gfortran/modules -I/usr/lib/gfortran/modules

noinst_LTLIBRARIES = libchimere2007lmsmr4.la libchimere2007lamaz.la libchimere2007lama3.la

libchimere2007lmsmr4_la_SOURCES = \
	modules/chimere_params.LMSMR4.F90 \
	modules/chimere_consts.F90 \
	diag/diagmet_common.F90 \
	diag/diagbio_common.F90 \
	diag/diagmet_science.F90 \
	diag/deepconvtiedke.F90 \
	diag/diagmet.F90 \
	tools/calendar.F90 \
	tools/subs.F90 \
	tools/io.F90 \
	tools/layave.F90

libchimere2007lamaz_la_SOURCES = \
	modules/chimere_params.LAMAZ.F90 \
	modules/chimere_consts.F90 \
	diag/diagmet_common.F90 \
	diag/diagbio_common.F90 \
	diag/diagmet_science.F90 \
	diag/deepconvtiedke.F90 \
	diag/diagmet.F90 \
	tools/calendar.F90 \
	tools/subs.F90 \
	tools/io.F90 \
	tools/layave.F90

libchimere2007lama3_la_SOURCES = \
	modules/chimere_params.LAMA3.F90 \
	modules/chimere_consts.F90 \
	diag/diagmet_common.F90 \
	diag/diagbio_common.F90 \
	diag/diagmet_science.F90 \
	diag/deepconvtiedke.F90 \
	diag/diagmet.F90 \
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
