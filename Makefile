# This is meant to be used on a Linux machine to generate
# the release archives for github. For normal development
# one would rather build and debug it from within the IDE.
#
# The Makefile requires Lazarus and FPC to be installed with
# native compiler and with cross compiler for Windows/x86-64.

LAZDIR ?= $(HOME)/laz-fixes/lazarus/
LAZBUILD = $(LAZDIR)/lazbuild

GITDESC = $(shell git describe --tags)
ifeq ($(GITDESC),)
    VERSION =
else
    VERSION = -$(GITDESC)
endif

LINARCHIVE = boundary-wire-linux64$(VERSION).tar.gz
WINARCHIVE = boundary-wire-windows64$(VERSION).zip

DEPS = *.pas *.lpi *.lpr *.ico Makefile

FILES = \
    models \
    boundary_visualizer.ini \

LINFILES = \
    boundary_visualizer \

WINFILES = \
    boundary_visualizer.exe \

all: linux windows
linux: $(LINARCHIVE)
windows: $(WINARCHIVE)

$(LINARCHIVE): $(DEPS)
	$(LAZBUILD) --build-mode='Linux Qt5' --ws=qt5 boundary_visualizer.lpi
	tar -czvf $(LINARCHIVE) $(FILES) $(LINFILES)

$(WINARCHIVE): $(DEPS)
	$(LAZBUILD) --build-mode='Windows 64' boundary_visualizer.lpi
	zip -r $(WINARCHIVE) $(FILES) $(WINFILES)

clean:
	rm -rf lib
	rm -f *.tar.gz
	rm -f *.zip
	rm -f *.exe
	rm -f *.res
	rm -f *.png
	rm -f debug.txt
	rm -f boundary_visualizer
