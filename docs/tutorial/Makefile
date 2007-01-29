
TOP = .

SUBDIRS = intro glade

default : tutorial.html
	for dir in $(SUBDIRS); do $(MAKE) -C $${dir}; done

EXTRA_CLEANFILES += exports $(TOOLSDIR)/AddLinks \
	$(TOOLSDIR)/AddLinks.o $(TOOLSDIR)/AddLinks.hi

include $(TOP)/common.mk
