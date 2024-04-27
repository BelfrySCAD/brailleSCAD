LIBLOUS = /usr/share/lib/liblous/tables

LIBLOUIS = /usr/share/liblouis/tables

CONVERT = ./liblouis2OpenSCAD.pl

en-us-g2.scad:
	$(CONVERT) $(LIBLOUIS)/en-us-g2.ctb > en-us-g2.scad

en-us-g1.scad:
	$(CONVERT) $(LIBLOUIS)/en-us-g1.ctb > en-us-g1.scad

