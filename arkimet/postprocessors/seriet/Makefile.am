postprocdir = $(libdir)/arkimet/

postproc_SCRIPTS = seriet

seriet: seriet.in
	sed -e "s,[@]pkglibexecdir[@],$(pkglibexecdir),g" $< > $@

EXTRA_DIST = seriet.in

CLEANFILES = seriet
