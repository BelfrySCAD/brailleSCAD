LIBLOUS = /usr/share/lib/liblous/tables

LIBLOUIS = /usr/share/liblouis/tables

CONVERT = ./liblouis2OpenSCAD.pl

%.scad : $(LIBLOUIS)/%.ctb $(CONVERT)
	$(CONVERT) $< > $@

all: en-us-g2.scad en-us-g1.scad

