AUTOMAKE_OPTIONS = 1.8 foreign no-dependencies
SUFFIXES = .hs.uncpp .hs.pp .chs.pp .chs .hsc .deps .p_deps .dep
MOSTLYCLEANFILES = */*.bak tools/*/*.bak
CLEANFILES = */*.precomp $(pkglib_LIBRARIES:.a=_p.a) */*.deps */*.p_deps
DISTCLEANFILES =

if GHC_VERSION_610
FFI = -XForeignFunctionInterface
else
FFI = -fffi
endif

EXTRA_DIST = \
	template-hsc-gtk2hs.h			\
	tools/callbackGen/Signal.chs.template	\
	tools/callbackGen/gtkmarshal.list	\
	tools/checkDirs.sh			\
	tools/c2hs/toplevel/c2hs_config.h	\
	tools/hierarchyGen/hierarchy.list	\
	tools/hierarchyGen/Hierarchy.chs.template \
        gconf/System/Gnome/GConf/marshal.list	\
	sourceview/sourceview.h			\
	gtksourceview2/gtksourceview2.h		\
	gtk/wingtk.h				\
        glib/System/Glib/hsgclosure.h		\
	cairo/COPYRIGHT				\
	cairo/cairo-gtk2hs.h			\
	svgcairo/svgcairo.h			\
	gtk/Graphics/UI/Gtk/General/hsgthread.h	\
	gnomevfs/hsgnomevfs-2.14.h		\
	gnomevfs/System/Gnome/VFS/hsfileinfo.h	\
	gnomevfs/marshal.list 			\
	gstreamer/hsgstreamer.h			\
	gstreamer/hierarchy.list		\
	gstreamer/mini-hierarchy.list		\
	gstreamer/gobject-hierarchy.list	\
	gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template	\
	gstreamer/Media/Streaming/GStreamer/MiniHierarchy.chs.template \
	gstreamer/marshal.list 			\
	gio/marshal.list			\
	gtksourceview2/marshal.list		\
	gtk/Graphics/UI/Gtk/ModelView/Gtk2HsStore.h	\
	COPYING.LGPLv3 COPYING.GPLv3

# There are no other files in these dirs and tar excludes empty dirs
dist-hook:
	mkdir $(distdir)/glade/Graphics/UI/Gtk/Glade
	touch $(distdir)/glade/Graphics/UI/Gtk/Glade/.keep
	mkdir $(distdir)/mozembed/Graphics/UI/Gtk/MozEmbed
	touch $(distdir)/mozembed/Graphics/UI/Gtk/MozEmbed/.keep

CHSDEPEND = $(srcdir)/mk/chsDepend

# all packages and applications
pkglib_LIBRARIES =	\
        libHSglib.a
if ENABLE_CAIRO
pkglib_LIBRARIES += libHScairo.a
endif
if ENABLE_SVGCAIRO
pkglib_LIBRARIES += libHSsvgcairo.a
endif
if ENABLE_GTK
pkglib_LIBRARIES +=	\
	libHSgtk.a	\
	libHSsoegtk.a
endif

if ENABLE_GIO
pkglib_LIBRARIES += libHSgio.a
endif
if ENABLE_LIBGLADE
pkglib_LIBRARIES += libHSglade.a
endif
if ENABLE_GCONF
pkglib_LIBRARIES += libHSgconf.a
endif
if ENABLE_SOURCEVIEW
pkglib_LIBRARIES += libHSsourceview.a
endif
if ENABLE_GTKSOURCEVIEW2
pkglib_LIBRARIES += libHSgtksourceview2.a
endif
if ENABLE_MOZEMBED
pkglib_LIBRARIES += libHSmozembed.a
endif
if ENABLE_GTKGLEXT
pkglib_LIBRARIES += libHSgtkglext.a
endif
if ENABLE_GNOMEVFS
pkglib_LIBRARIES += libHSgnomevfs.a
endif
if ENABLE_GSTREAMER
pkglib_LIBRARIES += libHSgstreamer.a
endif

noinst_PROGRAMS = \
	tools/hierarchyGen/TypeGenerator \
	tools/hierarchyGenGst/TypeGenerator \
	tools/callbackGen/HookGenerator \
	tools/c2hs/c2hsLocal

pkglibincludedir = $(pkglibdir)/include
pkglibinclude_DATA = gtk2hs-config.h

hidir = $(pkglibdir)/imports

# Sadly the generated Makefile is not safe for parallel builds
# so prevent people from shooting themselves in the foot:
.NOTPARALLEL:

#
#	TypeGenerator Tool
#
################################################################################

# While building the program <name>, set the variable NAME to <name> so
# we can access program-specific variables like <name>_EXTERNALDEPS. The
# following is a hack to prevent automake from assuming that we are overriding
# the tools/hierarchyGen/TypeGenerator goal.
tools_hierarchyGen_PKGNAME = tools_hierarchyGen_TypeGenerator

tools_hierarchyGen_TypeGenerator_MAIN = \
  $(tools_hierarchyGen_TypeGenerator_SOURCES)
tools_hierarchyGen_TypeGenerator_EXTERNALDEPS = base-$(PKG_BASE_VERSION)
tools_hierarchyGen_TypeGenerator_NOSPLITOBJS = yes
tools_hierarchyGen_TypeGenerator_SOURCESDIRS = tools/hierarchyGen
tools_hierarchyGen_TypeGenerator_SOURCES = \
  tools/hierarchyGen/TypeGen.hs
# Fix automake - the subdir-objects option doesn't work here.
am_tools_hierarchyGen_TypeGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_hierarchyGen_TypeGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_hierarchyGen_TypeGenerator_OBJECTS)
MOSTLYCLEANFILES+= $(tools_hierarchyGen_TypeGenerator_SOURCES:.hs=.hi)

tools_hierarchyGenGst_PKGNAME = tools_hierarchyGenGst_TypeGenerator

tools_hierarchyGenGst_TypeGenerator_MAIN = \
  $(tools_hierarchyGenGst_TypeGenerator_SOURCES)
tools_hierarchyGenGst_TypeGenerator_EXTERNALDEPS = base-$(PKG_BASE_VERSION)
tools_hierarchyGenGst_TypeGenerator_NOSPLITOBJS = yes
tools_hierarchyGenGst_TypeGenerator_SOURCESDIRS = tools/hierarchyGenGst
tools_hierarchyGenGst_TypeGenerator_SOURCES = \
  tools/hierarchyGenGst/TypeGen.hs
# Fix automake - the subdir-objects option doesn't work here.
am_tools_hierarchyGenGst_TypeGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_hierarchyGenGst_TypeGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_hierarchyGenGst_TypeGenerator_OBJECTS)
MOSTLYCLEANFILES+= $(tools_hierarchyGenGst_TypeGenerator_SOURCES:.hs=.hi)

#
#	HookGenerator Tool
#
################################################################################

tools_callbackGen_PKGNAME = tools_callbackGen_HookGenerator

tools_callbackGen_HookGenerator_MAIN = \
  $(tools_callbackGen_HookGenerator_SOURCES)
tools_callbackGen_HookGenerator_EXTERNALDEPS = base-$(PKG_BASE_VERSION)
tools_callbackGen_HookGenerator_NOSPLITOBJS = yes
tools_callbackGen_HookGenerator_SOURCESDIRS = tools/callbackGen
tools_callbackGen_HookGenerator_SOURCES = \
  tools/callbackGen/HookGenerator.hs
tools_callbackGen_HookGenerator_hs_HCFLAGS = -I. 
am_tools_callbackGen_HookGenerator_OBJECTS = $(addsuffix .$(OBJEXT),\
  $(basename $(tools_callbackGen_HookGenerator_SOURCES)))
MOSTLYCLEANFILES+= $(am_tools_callbackGen_HookGenerator_OBJECTS)
MOSTLYCLEANFILES+= $(tools_callbackGen_HookGenerator_SOURCES:.hs=.hi)

#
#	c2hs interface generator
#
################################################################################

tools_c2hs_PKGNAME = tools_c2hs_c2hsLocal

tools_c2hs_c2hsLocal_MAIN = c2hs/toplevel/Main.hs
tools_c2hs_c2hsLocal_EXTERNALDEPS	= base-$(PKG_BASE_VERSION) haskell98-$(PKG_HASKELL98_VERSION)
if HAVE_SPLIT_BASE
tools_c2hs_c2hsLocal_EXTERNALDEPS	+= pretty-$(PKG_PRETTY_VERSION) containers-$(PKG_CONTAINERS_VERSION) array-$(PKG_ARRAY_VERSION)
endif
tools_c2hs_c2hsLocal_NOSPLITOBJS	= yes

tools_c2hs_c2hsLocal_SOURCESDIRS = 			\
	tools/c2hs/base/admin tools/c2hs/base/errors	\
	tools/c2hs/base/general tools/c2hs/base/state	\
	tools/c2hs/base/syms tools/c2hs/base/syntax	\
	tools/c2hs/c tools/c2hs/chs tools/c2hs/gen	\
	tools/c2hs/state tools/c2hs/toplevel

tools_c2hs_c2hsLocal_SOURCES =			\
	tools/c2hs/base/admin/BaseVersion.hs	\
	tools/c2hs/base/admin/Config.hs		\
	tools/c2hs/base/errors/Errors.hs	\
	tools/c2hs/base/general/DLists.hs	\
	tools/c2hs/base/general/FileOps.hs	\
	tools/c2hs/base/general/FNameOps.hs	\
	tools/c2hs/base/general/Map.hs		\
	tools/c2hs/base/general/Position.hs	\
	tools/c2hs/base/general/Set.hs		\
	tools/c2hs/base/general/UNames.hs	\
	tools/c2hs/base/general/Binary.hs	\
	tools/c2hs/base/general/FastMutInt.hs	\
	tools/c2hs/base/state/CIO.hs		\
	tools/c2hs/base/state/StateBase.hs	\
	tools/c2hs/base/state/State.hs		\
	tools/c2hs/base/state/StateTrans.hs	\
	tools/c2hs/base/syms/Attributes.hs	\
	tools/c2hs/base/syms/Idents.hs		\
	tools/c2hs/base/syms/NameSpaces.hs	\
	tools/c2hs/base/syntax/Lexers.hs	\
	tools/c2hs/c/CAST.hs			\
	tools/c2hs/c/CAttrs.hs			\
	tools/c2hs/c/CBuiltin.hs		\
	tools/c2hs/c/C.hs			\
	tools/c2hs/c/CLexer.hs			\
	tools/c2hs/c/CNames.hs			\
	tools/c2hs/c/CParser.hs			\
	tools/c2hs/c/CParserMonad.hs		\
	tools/c2hs/c/CPretty.hs			\
	tools/c2hs/c/CTokens.hs			\
	tools/c2hs/c/CTrav.hs			\
	tools/c2hs/chs/CHS.hs			\
	tools/c2hs/chs/CHSLexer.hs		\
	tools/c2hs/gen/CInfo.hs			\
	tools/c2hs/gen/GBMonad.hs		\
	tools/c2hs/gen/GenBind.hs		\
	tools/c2hs/gen/GenHeader.hs		\
	tools/c2hs/state/C2HSState.hs		\
	tools/c2hs/state/Switches.hs		\
	tools/c2hs/toplevel/Main.hs		\
	tools/c2hs/toplevel/Version.hs		\
	tools/c2hs/toplevel/c2hs_config.c

nodist_tools_c2hs_c2hsLocal_SOURCES =		\
	tools/c2hs/toplevel/C2HSConfig.hs

tools_c2hs_c2hsLocal_ALLSOURCES =		\
	$(tools_c2hs_c2hsLocal_SOURCES)		\
	$(nodist_tools_c2hs_c2hsLocal_SOURCES)

tools_c2hs_base_general_Binary_hs_HCFLAGS = -O -funbox-strict-fields
tools_c2hs_base_general_FastMutInt_hs_HCFLAGS = -O
tools_c2hs_base_syntax_Parsers_hs_HCFLAGS = -fglasgow-exts
tools_c2hs_toplevel_C2HSConfig_hs_HCFLAGS = $(FFI) '-\#include<c2hs_config.h>'

tools/c2hs/c/CLexer.hs : tools/c2hs/c/CLexer.x
	$(ALEX) --ghc -o $@ $<

tools/c2hs/c/CParser.hs : tools/c2hs/c/CParser.y
	$(HAPPY) --ghc --array --coerce --strict -o $@ $<

MAINTAINERCLEANFILES =				\
	tools/c2hs/c/CParser.hs			\
	tools/c2hs/c/CLexer.hs

EXTRA_DIST +=					\
	tools/c2hs/c/CParser.y			\
	tools/c2hs/c/CLexer.x

tools_c2hs_c2hsLocal_HSFILES		= \
	$(filter %.hs,$(tools_c2hs_c2hsLocal_ALLSOURCES))
# Fix automake - the subdir-objects option doesn't work here.
am_tools_c2hs_c2hsLocal_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(tools_c2hs_c2hsLocal_ALLSOURCES)))
$(am_tools_c2hs_c2hsLocal_OBJECTS): | tools/c2hs/c2hsLocal.deps
tools/c2hs/c2hsLocal.deps: $(tools_c2hs_c2hsLocal_HSFILES)
c2hs_MOSTLYCLEANFILES  = $(am_tools_c2hs_c2hsLocal_OBJECTS)
c2hs_MOSTLYCLEANFILES += $(tools_c2hs_c2hsLocal_HSFILES:.hs=.hi)
c2hs_CLEANFILES = tools/c2hs/c2hsLocal.deps

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include tools/c2hs/c2hsLocal.deps
 endif

#
#	glib package
#
################################################################################

glib_PKGNAME = libHSglib_a

libHSglib_a_NAME		= glib
libHSglib_a_CONFIG		= glib/glib.package.conf
libHSglib_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSglib_a_INTERNALDEPS	= 
libHSglib_a_HEADER 		= glib-object.h
libHSglib_a_PRECOMP		= glib/glib.precomp
libHSglib_a_LIBS		= $(GLIB_LIBS)
libHSglib_a_HCFLAGS		= $(FFI)
libHSglib_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS))
libHSglib_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS))

libHSglib_a_SOURCESDIRS		= glib

glib/libHSglib_a.deps	: $(libHSglib_a_HSFILES)

libHSglib_a_SOURCES =	        		\
	glib/System/Glib.hs			\
	glib/System/Glib/FFI.hs.pp		\
	glib/System/Glib/UTFString.hs.pp	\
	glib/System/Glib/Types.chs		\
	glib/System/Glib/GType.chs		\
	glib/System/Glib/GTypeConstants.hsc	\
	glib/System/Glib/GValue.chs		\
	glib/System/Glib/GValueTypes.chs	\
	glib/System/Glib/GParameter.hsc		\
	glib/System/Glib/StoreValue.hsc		\
	glib/System/Glib/GObject.chs.pp		\
	glib/System/Glib/Properties.chs		\
	glib/System/Glib/Attributes.hs		\
	glib/System/Glib/GError.chs.pp		\
	glib/System/Glib/GList.chs		\
	glib/System/Glib/Signals.chs.pp		\
        glib/System/Glib/Flags.hs		\
	glib/System/Glib/MainLoop.chs.pp	\
	glib/System/Glib/GDateTime.chs.pp

htmldoc_HSFILES_HIDDEN =				\
	glib/System/Glib/FFI.hs			\
	glib/System/Glib/Types.hs

if USE_GCLOSUE_SIGNALS_IMPL
libHSglib_a_SOURCES += glib/System/Glib/hsgclosure.c
# make calling an undefined function an error so we can catch changes in the
# GHC RTS API:
glib_System_Glib_hsgclosure_c_CFLAGS = -Werror-implicit-function-declaration
endif

glib_System_Glib_Types_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_GError_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_Attributes_hs_HCFLAGS	= -fglasgow-exts
glib_System_Glib_Signals_hs_HCFLAGS	= '-\#include"hsgclosure.h"'

libHSglib_a_LIBADD  = 				\
	glib/System/Glib/GObject_stub.o		\
	glib/System/Glib/MainLoop_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSglib_a_LIBADD += glib/System/Glib/Signals_stub.o
endif

am_libHSglib_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSglib_a_SOURCES))))

libHSglib_a_HSPPFILES  = $(filter %.hs.pp, $(libHSglib_a_SOURCES))
libHSglib_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglib_a_SOURCES))
libHSglib_a_CHSFILES = \
  $(filter %.chs,$(libHSglib_a_SOURCES:.chs.pp=.chs))
libHSglib_a_CHSFILES_HS = $(libHSglib_a_CHSFILES:.chs=.hs)
libHSglib_a_HSCFILES    = $(filter %.hsc, $(libHSglib_a_SOURCES))
libHSglib_a_HSCFILES_HS = $(libHSglib_a_HSCFILES:.hsc=.hs)
libHSglib_a_BUILDSOURCES = \
	$(libHSglib_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSglib_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSglib_a_CHSFILES_HS) \
	$(libHSglib_a_HSCFILES_HS)
libHSglib_a_HSFILES  = \
	$(filter %.hs,$(libHSglib_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSglib_a_SOURCES))
libHSglib_a_CFILES  = $(filter %.c,$(libHSglib_a_SOURCES))

nobase_hi_DATA = $(libHSglib_a_HSFILES:.hs=.hi)

glib_MOSTLYCLEANFILES  = $(am_libHSglib_a_OBJECTS)
glib_MOSTLYCLEANFILES += $(libHSglib_a_HSFILES:.hs=.hi)
glib_MOSTLYCLEANFILES += \
	$(libHSglib_a_CHSFILES:.chs=.chi)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.h)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.o)	\
	$(libHSglib_a_CHSFILES:.chs=_stub.c)
glib_CLEANFILES  = $(libHSglib_a_BUILDSOURCES)
glib_CLEANFILES += $(libHSglib_a_CHSFILES_HS:.hs=.dep)
glib_CLEANFILES += \
	$(libHSglib_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))			\
	$(libHSglib_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSglib_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSglib_a_CHSFILES_HS): $(libHSglib_a_PRECOMP)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSglib_a_CHSFILES:.chs=.dep) glib/libHSglib_a.deps
 endif

if ENABLE_SPLITOBJS
libHSglib_a_AR = $(srcdir)/mk/link-splitobjs.sh
glib_CLEANFILES_SPLIT = $(libHSglib_a_HSFILES:.hs=_split/)
else
libHSglib_a_AR = $(AR) $(ARFLAGS)
endif

libHSglib_a_DEPENDENCIES = HSglib.o
pkglib_DATA = HSglib.o
HSglib.o : $(libHSglib_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSglib_a_OBJECTS) $(libHSglib_a_LIBADD)

if ENABLE_PROFILING
libHSglib_a_DEPENDENCIES += libHSglib_p.a
pkglib_DATA += libHSglib_p.a
libHSglib_p.a : $(libHSglib_a_HSFILES:.hs=.p_o) \
		$(libHSglib_a_CFILES:.c=.o) \
		$(libHSglib_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include glib/libHSglib_a.p_deps
 endif

nobase_hi_DATA        += $(libHSglib_a_HSFILES:.hs=.p_hi)
glib_MOSTLYCLEANFILES += $(libHSglib_a_HSFILES:.hs=.p_hi)
glib_MOSTLYCLEANFILES += $(libHSglib_a_HSFILES:.hs=.p_o)
glib_MOSTLYCLEANFILES += $(libHSglib_a_LIBADD:.o=.p_o)
endif

#
#	gio package
#
################################################################################

if ENABLE_GIO

gio_PKGNAME = libHSgio_a

libHSgio_a_NAME			= gio
libHSgio_a_CONFIG		= gio/gio.package.conf
libHSgio_a_EXTERNALDEPS		= base-$(PKG_BASE_VERSION)
if HAVE_SPLIT_BASE
libHSgio_a_EXTERNALDEPS		+= bytestring-$(PKG_BYTESTRING_VERSION)
endif
libHSgio_a_INTERNALDEPS		= glib
libHSgio_a_HEADER 		= gio/gio.h
libHSgio_a_PRECOMP		= gio/gio.precomp
libHSgio_a_LIBS			= $(GLIB_LIBS) $(GIO_LIBS)
libHSgio_a_HCFLAGS		= $(FFI) -fglasgow-exts
libHSgio_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GIO_CFLAGS))
libHSgio_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GIO_CFLAGS))

libHSgio_a_SOURCESDIRS		= gio

gio/libHSgio_a.deps	: $(libHSgio_a_HSFILES) | glib/libHSglib_a.deps

libHSgio_a_GENERATEDSOURCES =			\
	gio/System/GIO/Types.chs		\
	gio/System/GIO/Signals.chs

nodist_libHSgio_a_SOURCES = $(libHSgio_a_GENERATEDSOURCES)

libHSgio_a_SOURCES =	        		\
	gio/System/GIO/AsyncResult.chs		\
	gio/System/GIO/Base.chs			\
	gio/System/GIO/Cancellable.chs		\
	gio/System/GIO/FileAttribute.hsc	\
	gio/System/GIO/File.chs			\
	gio/System/GIO.hs

htmldoc_HSFILES_HIDDEN +=			\
	$(libHSgio_a_GENERATEDSOURCES:.chs=.hs)

libHSgio_a_LIBADD  = 				\
	gio/System/GIO/Base_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgio_a_LIBADD +=	\
	gio/System/GIO/Signals_stub.o
endif

gio/System/GIO/Types.chs : \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gio --lib=gio --prefix=g \
	--modname=System.GIO.Types \
	--parentname=System.Glib.GObject)

gio/System/GIO/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/gio/marshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/gio/marshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	System.GIO.Signals)

libHSgio_a_ALLSOURCES = $(libHSgio_a_SOURCES) $(nodist_libHSgio_a_SOURCES)

am_libHSgio_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgio_a_ALLSOURCES))))

libHSgio_a_HSPPFILES  = $(filter %.hs.pp, $(libHSgio_a_ALLSOURCES))
libHSgio_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgio_a_ALLSOURCES))
libHSgio_a_CHSFILES = \
  $(filter %.chs,$(libHSgio_a_ALLSOURCES:.chs.pp=.chs))
libHSgio_a_CHSFILES_HS = $(libHSgio_a_CHSFILES:.chs=.hs)
libHSgio_a_HSCFILES    = $(filter %.hsc, $(libHSgio_a_ALLSOURCES))
libHSgio_a_HSCFILES_HS = $(libHSgio_a_HSCFILES:.hsc=.hs)
libHSgio_a_BUILDSOURCES = \
	$(libHSgio_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSgio_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgio_a_CHSFILES_HS) \
	$(libHSgio_a_HSCFILES_HS) \
	$(libHSgio_a_GENERATEDSOURCES)

libHSgio_a_HSFILES  = \
	$(filter %.hs,$(libHSgio_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgio_a_ALLSOURCES))
libHSgio_a_CFILES  = $(filter %.c,$(libHSgio_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgio_a_HSFILES:.hs=.hi)

gio_MOSTLYCLEANFILES  = $(am_libHSgio_a_OBJECTS)
gio_MOSTLYCLEANFILES += $(libHSgio_a_HSFILES:.hs=.hi)
gio_MOSTLYCLEANFILES += \
	$(libHSgio_a_CHSFILES:.chs=.chi)	\
	$(libHSgio_a_CHSFILES:.chs=.h)	\
	$(libHSgio_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgio_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgio_a_CHSFILES:.chs=_stub.c)
gio_CLEANFILES  = $(libHSgio_a_BUILDSOURCES)
gio_CLEANFILES += $(libHSgio_a_CHSFILES_HS:.hs=.dep)
gio_CLEANFILES += \
	$(libHSgio_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))	\
	$(libHSgio_a_HSCFILES:.hsc=_hsc_make.c)	\
	$(libHSgio_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgio_a_CHSFILES_HS): $(libHSgio_a_PRECOMP)
$(libHSgio_a_CHSFILES:.chs=.dep) :	\
	$(libHSgio_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgio_a_CHSFILES:.chs=.dep) gio/libHSgio_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgio_a_AR = $(srcdir)/mk/link-splitobjs.sh
gio_CLEANFILES_SPLIT = $(libHSgio_a_HSFILES:.hs=_split/)
else
libHSgio_a_AR = $(AR) $(ARFLAGS)
endif

libHSgio_a_DEPENDENCIES = HSgio.o
pkglib_DATA += HSgio.o
HSgio.o : $(libHSgio_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgio_a_OBJECTS) $(libHSgio_a_LIBADD)

if ENABLE_PROFILING
libHSgio_a_DEPENDENCIES += libHSgio_p.a
pkglib_DATA += libHSgio_p.a
libHSgio_p.a : $(libHSgio_a_HSFILES:.hs=.p_o) \
		$(libHSgio_a_CFILES:.c=.o) \
		$(libHSgio_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gio/libHSgio_a.p_deps
 endif

nobase_hi_DATA        += $(libHSgio_a_HSFILES:.hs=.p_hi)
gio_MOSTLYCLEANFILES += $(libHSgio_a_HSFILES:.hs=.p_hi)
gio_MOSTLYCLEANFILES += $(libHSgio_a_HSFILES:.hs=.p_o)
gio_MOSTLYCLEANFILES += $(libHSgio_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_GIO

#
#	gtk package
#
################################################################################

if ENABLE_GTK

gtk_PKGNAME = libHSgtk_a

libHSgtk_a_NAME			= gtk
libHSgtk_a_CONFIG		= gtk/gtk.package.conf
libHSgtk_a_EXTERNALDEPS		= base-$(PKG_BASE_VERSION) mtl-$(PKG_MTL_VERSION)
if HAVE_SPLIT_BASE
libHSgtk_a_EXTERNALDEPS		+= containers-$(PKG_CONTAINERS_VERSION) array-$(PKG_ARRAY_VERSION)
endif
libHSgtk_a_INTERNALDEPS		= glib
libHSgtk_a_HEADER 		= gtk/gtk.h
libHSgtk_a_PRECOMP		= gtk/gtk.precomp
libHSgtk_a_LIBS			= $(GLIB_LIBS) $(GTK_LIBS)
libHSgtk_a_HCFLAGS		= $(FFI)
libHSgtk_a_CFLAGS 		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))
libHSgtk_a_CPPFLAGS 		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS))

libHSgtk_a_SOURCESDIRS		= gtk

gtk/libHSgtk_a.deps	: $(libHSgtk_a_HSFILES) | glib/libHSglib_a.deps

if ENABLE_CAIRO
libHSgtk_a_INTERNALDEPS += cairo
gtk/libHSgtk_a.deps	: | cairo/libHScairo_a.deps
endif

libHSgtk_a_GENERATEDSOURCES =						\
	gtk/Graphics/UI/Gtk/Types.chs					\
	gtk/Graphics/UI/Gtk/Signals.chs

nodist_libHSgtk_a_SOURCES = $(libHSgtk_a_GENERATEDSOURCES)

libHSgtk_a_SOURCES =					 		\
	gtk/Graphics/UI/Gtk.hs.pp					\
	gtk/Graphics/UI/Gtk/Pango/Types.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Structs.hsc				\
	gtk/Graphics/UI/Gtk/Multiline/Types.chs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Region.chs.pp				\
	gtk/Graphics/UI/Gtk/Abstract/Bin.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Box.chs				\
	gtk/Graphics/UI/Gtk/Abstract/ButtonBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Container.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/ContainerChildProperties.chs	\
	gtk/Graphics/UI/Gtk/Abstract/Misc.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Object.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Paned.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Range.chs.pp			\
	gtk/Graphics/UI/Gtk/Abstract/Scale.chs				\
	gtk/Graphics/UI/Gtk/Abstract/Widget.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/Button.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/CheckButton.chs			\
	gtk/Graphics/UI/Gtk/Buttons/RadioButton.chs.pp			\
	gtk/Graphics/UI/Gtk/Buttons/ToggleButton.chs			\
	gtk/Graphics/UI/Gtk/Display/AccelLabel.chs			\
	gtk/Graphics/UI/Gtk/Display/Image.chs.pp			\
	gtk/Graphics/UI/Gtk/Display/Label.chs.pp			\
	gtk/Graphics/UI/Gtk/Display/ProgressBar.chs.pp			\
	gtk/Graphics/UI/Gtk/Display/Statusbar.chs			\
	gtk/Graphics/UI/Gtk/Display/StatusIcon.chs.pp			\
	gtk/Graphics/UI/Gtk/Embedding/Plug.chs.pp			\
	gtk/Graphics/UI/Gtk/Embedding/Socket.chs.pp			\
	gtk/Graphics/UI/Gtk/Embedding/Embedding.hsc			\
	gtk/Graphics/UI/Gtk/Entry/Editable.chs.pp			\
	gtk/Graphics/UI/Gtk/Entry/Entry.chs.pp				\
	gtk/Graphics/UI/Gtk/Entry/EntryCompletion.chs.pp		\
	gtk/Graphics/UI/Gtk/Entry/HScale.chs				\
	gtk/Graphics/UI/Gtk/Entry/SpinButton.chs			\
	gtk/Graphics/UI/Gtk/Entry/VScale.chs				\
	gtk/Graphics/UI/Gtk/General/Clipboard.chs.pp			\
	gtk/Graphics/UI/Gtk/General/Enums.chs.pp			\
	gtk/Graphics/UI/Gtk/General/General.chs				\
	gtk/Graphics/UI/Gtk/General/hsgthread.c				\
	gtk/Graphics/UI/Gtk/General/IconFactory.chs.pp			\
	gtk/Graphics/UI/Gtk/General/Style.chs				\
	gtk/Graphics/UI/Gtk/Layout/Alignment.chs.pp			\
	gtk/Graphics/UI/Gtk/Layout/AspectFrame.chs			\
	gtk/Graphics/UI/Gtk/Layout/Expander.chs.pp			\
	gtk/Graphics/UI/Gtk/Layout/Fixed.chs				\
	gtk/Graphics/UI/Gtk/Layout/HBox.chs				\
	gtk/Graphics/UI/Gtk/Layout/HButtonBox.chs			\
	gtk/Graphics/UI/Gtk/Layout/HPaned.chs				\
	gtk/Graphics/UI/Gtk/Layout/Layout.chs				\
	gtk/Graphics/UI/Gtk/Layout/Notebook.chs.pp			\
	gtk/Graphics/UI/Gtk/Layout/Table.chs				\
	gtk/Graphics/UI/Gtk/Layout/VBox.chs				\
	gtk/Graphics/UI/Gtk/Layout/VButtonBox.chs			\
	gtk/Graphics/UI/Gtk/Layout/VPaned.chs				\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Combo.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ComboBox.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ComboBoxEntry.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Menu.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuBar.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuShell.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/TearoffMenuItem.chs	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/Toolbar.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToolButton.chs.pp		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ToggleToolButton.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/SeparatorMenuItem.chs.pp	\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/SeparatorToolItem.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/Action.chs.pp		\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/ActionGroup.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs.pp	\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs.pp		\
	gtk/Graphics/UI/Gtk/Misc/Adjustment.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Calendar.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/DrawingArea.chs			\
	gtk/Graphics/UI/Gtk/Misc/EventBox.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Arrow.chs				\
	gtk/Graphics/UI/Gtk/Misc/HandleBox.chs				\
	gtk/Graphics/UI/Gtk/Misc/SizeGroup.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Tooltips.chs.pp			\
	gtk/Graphics/UI/Gtk/Misc/Viewport.chs				\
	gtk/Graphics/UI/Gtk/Multiline/TextBuffer.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextIter.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextMark.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextTag.chs.pp			\
	gtk/Graphics/UI/Gtk/Multiline/TextTagTable.chs			\
	gtk/Graphics/UI/Gtk/Multiline/TextView.chs.pp			\
	gtk/Graphics/UI/Gtk/Ornaments/Frame.chs				\
	gtk/Graphics/UI/Gtk/Ornaments/HSeparator.chs			\
	gtk/Graphics/UI/Gtk/Ornaments/VSeparator.chs			\
	gtk/Graphics/UI/Gtk/Scrolling/HScrollbar.chs			\
	gtk/Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs.pp		\
	gtk/Graphics/UI/Gtk/Scrolling/VScrollbar.chs			\
	gtk/Graphics/UI/Gtk/Selectors/ColorButton.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/ColorSelection.chs		\
	gtk/Graphics/UI/Gtk/Selectors/ColorSelectionDialog.chs		\
	gtk/Graphics/UI/Gtk/Selectors/FontButton.chs.pp			\
	gtk/Graphics/UI/Gtk/Selectors/FontSelection.chs			\
	gtk/Graphics/UI/Gtk/Selectors/FontSelectionDialog.chs		\
	gtk/Graphics/UI/Gtk/Selectors/FileSelection.chs.pp              \
	gtk/Graphics/UI/Gtk/Selectors/FileChooser.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserButton.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserWidget.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileChooserDialog.chs.pp		\
	gtk/Graphics/UI/Gtk/Selectors/FileFilter.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList.hs					\
	gtk/Graphics/UI/Gtk/TreeList/CellRenderer.hs			\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererPixbuf.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererText.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellRendererToggle.chs		\
	gtk/Graphics/UI/Gtk/TreeList/CellView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/IconView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/ListStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeIter.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeModelSort.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreePath.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeStore.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/TreeList/TreeViewColumn.chs.pp		\
	gtk/Graphics/UI/Gtk/TreeList/TreeRowReference.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView.hs				\
	gtk/Graphics/UI/Gtk/ModelView/CellLayout.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/CellRenderer.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererCombo.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererPixbuf.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererProgress.chs.pp	\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererText.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellRendererToggle.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/CellView.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/CustomStore.chs			\
	gtk/Graphics/UI/Gtk/ModelView/Gtk2HsStore.c			\
	gtk/Graphics/UI/Gtk/ModelView/IconView.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/ListStore.hs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/Sequence.hs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/TreeDrag.chs			\
	gtk/Graphics/UI/Gtk/ModelView/TreeModel.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/TreeModelSort.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/TreeSortable.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/TreeSelection.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/TreeStore.hs			\
	gtk/Graphics/UI/Gtk/ModelView/TreeView.chs.pp			\
	gtk/Graphics/UI/Gtk/ModelView/TreeViewColumn.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/TreeRowReference.chs.pp		\
	gtk/Graphics/UI/Gtk/ModelView/Types.chs				\
	gtk/Graphics/UI/Gtk/Windows/AboutDialog.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/Dialog.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/MessageDialog.chs.pp		\
	gtk/Graphics/UI/Gtk/Windows/Window.chs.pp			\
	gtk/Graphics/UI/Gtk/Windows/WindowGroup.chs			\
	gtk/Graphics/UI/Gtk/Gdk/Cursor.chs				\
	gtk/Graphics/UI/Gtk/Gdk/Drawable.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/GC.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/Gdk.chs					\
	gtk/Graphics/UI/Gtk/Gdk/Enums.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/Keys.chs				\
	gtk/Graphics/UI/Gtk/Gdk/PixbufData.hs.pp			\
	gtk/Graphics/UI/Gtk/Gdk/Pixbuf.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/Pixmap.chs.pp				\
	gtk/Graphics/UI/Gtk/Gdk/Screen.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Attributes.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Description.chs			\
	gtk/Graphics/UI/Gtk/Pango/Font.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Enums.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/GlyphStorage.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Context.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Layout.chs.pp				\
	gtk/Graphics/UI/Gtk/Pango/Rendering.chs.pp			\
	gtk/Graphics/UI/Gtk/General/StockItems.hsc			\
	gtk/Graphics/UI/Gtk/General/Structs.hsc				\
	gtk/Graphics/UI/Gtk/Gdk/Events.hsc				\
	gtk/Graphics/UI/Gtk/Gdk/EventM.hsc				\
	gtk/Graphics/UI/Gtk/Abstract/Scrollbar.hs			\
	gtk/Graphics/UI/Gtk/Abstract/Separator.hs			\
	gtk/Graphics/UI/Gtk/Gdk/DrawWindow.chs.pp			\
	gtk/Graphics/UI/Gtk/Pango/Markup.hs.pp				\
	gtk/Graphics/UI/Gtk/General/Selection.chs.pp \
	gtk/Graphics/UI/Gtk/General/Drag.chs.pp \
	gtk/Graphics/UI/Gtk/General/DNDTypes.chs

if ENABLE_CAIRO
libHSgtk_a_SOURCES +=					 		\
	gtk/Graphics/UI/Gtk/Cairo.chs.pp
endif

htmldoc_HSFILES_HIDDEN +=						\
	$(libHSgtk_a_GENERATEDSOURCES:.chs=.hs)			\
	gtk/Graphics/UI/Gtk/Gdk/PixbufData.hs			\
	gtk/Graphics/UI/Gtk/General/Structs.hs			\
	gtk/Graphics/UI/Gtk/Pango/Description.hs		\
	gtk/Graphics/UI/Gtk/Pango/Types.hs			\
	gtk/Graphics/UI/Gtk/Pango/Structs.hs			\
	gtk/Graphics/UI/Gtk/Pango/Attributes.hs			\
	gtk/Graphics/UI/Gtk/Pango/GlyphStorage.hs		\
	gtk/Graphics/UI/Gtk/TreeList/TreePath.hs		\
	gtk/Graphics/UI/Gtk/ModelView/Types.hs			\
	gtk/Graphics/UI/Gtk/ModelView/Sequence.hs		\
	gtk/Graphics/UI/Gtk/Multiline/Types.hs			\
	gtk/Graphics/UI/Gtk/Abstract/ContainerChildProperties.hs \
	gtk/Graphics/UI/Gtk/General/DNDTypes.hs

gtk_Graphics_UI_Gtk_hs_HCFLAGS			= -fno-warn-duplicate-exports	#FIXME
gtk_Graphics_UI_Gtk_Gdk_Enums_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Types_hs_HCFLAGS	        = -fglasgow-exts
gtk_Graphics_UI_Gtk_General_Structs_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Events_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_EventM_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_General_StockItems_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Region_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_Drawable_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_ModelView_Types_hs_HCFLAGS	= -fglasgow-exts
gtk_Graphics_UI_Gtk_Gdk_PixbufData_hs_HCFLAGS	= -fglasgow-exts

if WIN32
gtk_Graphics_UI_Gtk_General_General_hs_HCFLAGS	= '-\#include"gtk/wingtk.h"' -I.
endif

libHSgtk_a_LIBADD =							\
	gtk/Graphics/UI/Gtk/General/Clipboard_stub.o			\
	gtk/Graphics/UI/Gtk/Abstract/Container_stub.o			\
	gtk/Graphics/UI/Gtk/Multiline/TextIter_stub.o			\
        gtk/Graphics/UI/Gtk/Multiline/TextTagTable_stub.o		\
	gtk/Graphics/UI/Gtk/MenuComboToolbar/ComboBox_stub.o		\
        gtk/Graphics/UI/Gtk/TreeList/TreeModel_stub.o			\
        gtk/Graphics/UI/Gtk/TreeList/TreeSelection_stub.o		\
        gtk/Graphics/UI/Gtk/TreeList/TreeView_stub.o			\
        gtk/Graphics/UI/Gtk/ModelView/TreeModel_stub.o			\
        gtk/Graphics/UI/Gtk/ModelView/TreeSelection_stub.o		\
        gtk/Graphics/UI/Gtk/ModelView/TreeView_stub.o			\
	gtk/Graphics/UI/Gtk/ModelView/TreeSortable_stub.o		\
	gtk/Graphics/UI/Gtk/ModelView/CustomStore_stub.o

if HAVE_GTK_VERSION_2_4
libHSgtk_a_LIBADD +=							\
	gtk/Graphics/UI/Gtk/Entry/EntryCompletion_stub.o		\
	gtk/Graphics/UI/Gtk/ModelView/CellLayout_stub.o			\
	gtk/Graphics/UI/Gtk/Selectors/FileFilter_stub.o			\
	gtk/Graphics/UI/Gtk/ActionMenuToolbar/ActionGroup_stub.o
endif

if HAVE_GTK_VERSION_2_6
libHSgtk_a_LIBADD +=							\
	gtk/Graphics/UI/Gtk/TreeList/IconView_stub.o			\
	gtk/Graphics/UI/Gtk/ModelView/IconView_stub.o			\
	gtk/Graphics/UI/Gtk/Windows/AboutDialog_stub.o
endif

if ENABLE_CAIRO
if HAVE_GTK_VERSION_2_8
libHSgtk_a_LIBADD +=							\
	gtk/Graphics/UI/Gtk/Cairo_stub.o                
endif
endif

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgtk_a_LIBADD += gtk/Graphics/UI/Gtk/Signals_stub.o
endif

gtk/Graphics/UI/Gtk/Types.chs : \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
        $(srcdir)/tools/hierarchyGen/hierarchy.list \
        $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
        $@ $(addprefix --tag=,$(CREATE_TYPES)) \
	--modname=Graphics.UI.Gtk.Types \
        --parentname=System.Glib.GObject)

gtk/Graphics/UI/Gtk/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/tools/callbackGen/gtkmarshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/tools/callbackGen/gtkmarshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	Graphics.UI.Gtk.Signals)

libHSgtk_a_ALLSOURCES = $(libHSgtk_a_SOURCES) $(nodist_libHSgtk_a_SOURCES)

am_libHSgtk_a_OBJECTS = \
  $(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtk_a_ALLSOURCES))))

libHSgtk_a_HSPPFILES    = $(filter %.hs.pp, $(libHSgtk_a_ALLSOURCES))
libHSgtk_a_CHSPPFILES   = $(filter %.chs.pp,$(libHSgtk_a_ALLSOURCES))
libHSgtk_a_CHSFILES     = $(filter %.chs,$(libHSgtk_a_ALLSOURCES:.chs.pp=.chs))
libHSgtk_a_CHSFILES_HS  = $(libHSgtk_a_CHSFILES:.chs=.hs)
libHSgtk_a_HSCFILES     = $(filter %.hsc, $(libHSgtk_a_ALLSOURCES))
libHSgtk_a_HSCFILES_HS  = $(libHSgtk_a_HSCFILES:.hsc=.hs)

libHSgtk_a_BUILDSOURCES = \
	$(libHSgtk_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSgtk_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtk_a_CHSFILES_HS) \
	$(libHSgtk_a_HSCFILES_HS) \
	$(libHSgtk_a_GENERATEDSOURCES)
libHSgtk_a_HSFILES  = \
	$(filter %.hs,$(libHSgtk_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtk_a_ALLSOURCES))
libHSgtk_a_CFILES   = $(filter %.c,$(libHSgtk_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgtk_a_HSFILES:.hs=.hi)

gtk_MOSTLYCLEANFILES  = $(am_libHSgtk_a_OBJECTS)
gtk_MOSTLYCLEANFILES += $(libHSgtk_a_HSFILES:.hs=.hi)
gtk_MOSTLYCLEANFILES += \
	$(libHSgtk_a_CHSFILES:.chs=.chi)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgtk_a_CHSFILES:.chs=_stub.c)
gtk_CLEANFILES  = $(libHSgtk_a_BUILDSOURCES)
gtk_CLEANFILES += $(libHSgtk_a_CHSFILES_HS:.hs=.dep)
gtk_CLEANFILES += \
	$(libHSgtk_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))	\
	$(libHSgtk_a_HSCFILES:.hsc=_hsc_make.c)	\
	$(libHSgtk_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgtk_a_CHSFILES_HS): $(libHSgtk_a_PRECOMP)
$(libHSgtk_a_CHSFILES:.chs=.dep) : $(libHSgtk_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgtk_a_CHSFILES_HS:.hs=.dep) gtk/libHSgtk_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgtk_a_AR = $(srcdir)/mk/link-splitobjs.sh
gtk_CLEANFILES_SPLIT = $(libHSgtk_a_HSFILES:.hs=_split/)
else
libHSgtk_a_AR = $(AR) $(ARFLAGS)
endif

libHSgtk_a_DEPENDENCIES = HSgtk.o
pkglib_DATA += HSgtk.o
HSgtk.o : $(libHSgtk_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgtk_a_OBJECTS) $(libHSgtk_a_LIBADD)

if ENABLE_PROFILING
libHSgtk_a_DEPENDENCIES += libHSgtk_p.a
pkglib_DATA += libHSgtk_p.a
libHSgtk_p.a : $(libHSgtk_a_HSFILES:.hs=.p_o) \
		$(libHSgtk_a_CFILES:.c=.o) \
		$(libHSgtk_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gtk/libHSgtk_a.p_deps
 endif

nobase_hi_DATA       += $(libHSgtk_a_HSFILES:.hs=.p_hi)
gtk_MOSTLYCLEANFILES += $(libHSgtk_a_HSFILES:.hs=.p_hi)
gtk_MOSTLYCLEANFILES += $(libHSgtk_a_HSFILES:.hs=.p_o)
gtk_MOSTLYCLEANFILES += $(libHSgtk_a_LIBADD:.o=.p_o)
endif

endif

#
#	glade package
#
################################################################################

if ENABLE_LIBGLADE

glade_PKGNAME = libHSglade_a

libHSglade_a_NAME		= glade
libHSglade_a_CONFIG		= glade/glade.package.conf
libHSglade_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSglade_a_INTERNALDEPS	= gtk glib
libHSglade_a_HEADER 		= glade/glade.h
libHSglade_a_PRECOMP		= glade/glade.precomp
libHSglade_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(LIBGLADE_LIBS)
libHSglade_a_HCFLAGS		= $(FFI)
libHSglade_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))
libHSglade_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(LIBGLADE_CFLAGS))

libHSglade_a_SOURCESDIRS	= glade

glade/libHSglade_a.deps	: $(libHSglade_a_HSFILES) | gtk/libHSgtk_a.deps

libHSglade_a_GENERATEDSOURCES =	\
	glade/Graphics/UI/Gtk/Glade/Types.chs

nodist_libHSglade_a_SOURCES = $(libHSglade_a_GENERATEDSOURCES)

libHSglade_a_SOURCES =	        \
        glade/Graphics/UI/Gtk/Glade.chs

htmldoc_HSFILES_HIDDEN += $(libHSglade_a_GENERATEDSOURCES:.chs=.hs)

glade_Graphics_UI_Gtk_Glade_Types_hs_HCFLAGS	= -fglasgow-exts

glade/Graphics/UI/Gtk/Glade/Types.chs : $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
		      $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
		      $(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
		      $(srcdir)/tools/hierarchyGen/hierarchy.list \
		      $(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
		      $@ --tag=libglade --lib=glade --prefix=glade \
                      --modname=Graphics.UI.Gtk.Glade.Types \
		      --parentname=Graphics.UI.Gtk.Types)

libHSglade_a_ALLSOURCES = $(libHSglade_a_SOURCES) $(nodist_libHSglade_a_SOURCES)

am_libHSglade_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSglade_a_ALLSOURCES))))

libHSglade_a_HSPPFILES  = $(filter %.hs.pp, $(libHSglade_a_ALLSOURCES))
libHSglade_a_CHSPPFILES = $(filter %.chs.pp,$(libHSglade_a_ALLSOURCES))
libHSglade_a_CHSFILES = \
  $(filter %.chs,$(libHSglade_a_ALLSOURCES:.chs.pp=.chs))
libHSglade_a_CHSFILES_HS = $(libHSglade_a_CHSFILES:.chs=.hs)
libHSglade_a_HSCFILES = $(filter %.hsc, $(libHSglade_a_ALLSOURCES))
libHSglade_a_HSCFILES_HS = $(libHSglade_a_HSCFILES:.hsc=.hs)
libHSglade_a_BUILDSOURCES = \
	$(libHSglade_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSglade_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSglade_a_CHSFILES_HS) \
        $(libHSglade_a_HSCFILES_HS) \
        $(libHSglade_a_GENERATEDSOURCES)
libHSglade_a_HSFILES  = \
	$(filter %.hs,$(libHSglade_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSglade_a_ALLSOURCES))

nobase_hi_DATA += $(libHSglade_a_HSFILES:.hs=.hi)

glade_MOSTLYCLEANFILES  = $(am_libHSglade_a_OBJECTS)
glade_MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.hi)
glade_MOSTLYCLEANFILES += $(libHSglade_a_CHSFILES:.chs=.chi)
glade_CLEANFILES  = $(libHSglade_a_BUILDSOURCES)
glade_CLEANFILES += $(libHSglade_a_CHSFILES_HS:.hs=.dep)
glade_CLEANFILES += \
	$(libHSglade_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))	\
	$(libHSglade_a_HSCFILES:.hsc=_hsc_make.c)		\
	$(libHSglade_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSglade_a_CHSFILES_HS): $(libHSglade_a_PRECOMP)
$(libHSglade_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSglade_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSglade_a_CHSFILES:.chs=.dep) glade/libHSglade_a.deps
 endif

if ENABLE_SPLITOBJS
libHSglade_a_AR = $(srcdir)/mk/link-splitobjs.sh
glade_CLEANFILES_SPLIT = $(libHSglade_a_HSFILES:.hs=_split/)
else
libHSglade_a_AR = $(AR) $(ARFLAGS)
endif

libHSglade_a_DEPENDENCIES = HSglade.o
pkglib_DATA += HSglade.o
HSglade.o : $(libHSglade_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSglade_a_OBJECTS) $(libHSglade_a_LIBADD)

if ENABLE_PROFILING
libHSglade_a_DEPENDENCIES += libHSglade_p.a
pkglib_DATA += libHSglade_p.a
libHSglade_p.a : $(libHSglade_a_HSFILES:.hs=.p_o) \
		$(libHSglade_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include glade/libHSglade_a.p_deps
 endif

nobase_hi_DATA         += $(libHSglade_a_HSFILES:.hs=.p_hi)
glade_MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.p_hi)
glade_MOSTLYCLEANFILES += $(libHSglade_a_HSFILES:.hs=.p_o)
glade_MOSTLYCLEANFILES += $(libHSglade_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_LIBGLADE

#
#	gconf package
#
################################################################################

if ENABLE_GCONF

gconf_PKGNAME = libHSgconf_a

libHSgconf_a_NAME		= gconf
libHSgconf_a_CONFIG		= gconf/gconf.package.conf
libHSgconf_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSgconf_a_INTERNALDEPS	= glib
libHSgconf_a_HEADER 		= gconf/gconf-client.h
libHSgconf_a_PRECOMP		= gconf/gconf.precomp
libHSgconf_a_LIBS		= $(GLIB_LIBS) $(GCONF_LIBS)
libHSgconf_a_HCFLAGS		= -fglasgow-exts
if GHC_VERSION_610
libHSgconf_a_HCFLAGS		+= -XOverlappingInstances
else
libHSgconf_a_HCFLAGS		+= -fallow-overlapping-instances
endif
libHSgconf_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GCONF_CFLAGS))
libHSgconf_a_CPPFLAGS		= $(filter -I% -D%,$(GLIB_CFLAGS) $(GCONF_CFLAGS))

libHSgconf_a_SOURCESDIRS	= gconf

gconf/libHSgconf_a.deps	: $(libHSgconf_a_HSFILES) | glib/libHSglib_a.deps

libHSgconf_a_GENERATEDSOURCES =				\
	gconf/System/Gnome/GConf/Types.chs		\
	gconf/System/Gnome/GConf/Signals.chs

nodist_libHSgconf_a_SOURCES = $(libHSgconf_a_GENERATEDSOURCES)

libHSgconf_a_SOURCES =					\
	gconf/System/Gnome/GConf/GConfValue.chs		\
	gconf/System/Gnome/GConf/GConfClient.chs	\
	gconf/System/Gnome/GConf.hs

htmldoc_HSFILES_HIDDEN += $(libHSgconf_a_GENERATEDSOURCES:.chs=.hs)

libHSgconf_a_LIBADD =	\
	gconf/System/Gnome/GConf/GConfClient_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgconf_a_LIBADD +=	\
	gconf/System/Gnome/GConf/Signals_stub.o
endif

gconf/System/Gnome/GConf/Types.chs :			\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gconf --lib=gconf --prefix=gconf \
	--modname=System.Gnome.GConf.Types --parentname=System.Glib.GObject)

gconf/System/Gnome/GConf/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/gconf/System/Gnome/GConf/marshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/gconf/System/Gnome/GConf/marshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	System.Gnome.GConf.Signals)

libHSgconf_a_ALLSOURCES = $(libHSgconf_a_SOURCES) $(nodist_libHSgconf_a_SOURCES)

am_libHSgconf_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgconf_a_ALLSOURCES))))

libHSgconf_a_HSPPFILES  = $(filter %.hs.pp, $(libHSgconf_a_ALLSOURCES))
libHSgconf_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgconf_a_ALLSOURCES))
libHSgconf_a_CHSFILES = \
  $(filter %.chs,$(libHSgconf_a_ALLSOURCES:.chs.pp=.chs))
libHSgconf_a_CHSFILES_HS = $(libHSgconf_a_CHSFILES:.chs=.hs)
libHSgconf_a_HSCFILES = $(filter %.hsc, $(libHSgconf_a_ALLSOURCES))
libHSgconf_a_HSCFILES_HS = $(libHSgconf_a_HSCFILES:.hsc=.hs)
libHSgconf_a_BUILDSOURCES = \
	$(libHSgconf_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSgconf_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgconf_a_CHSFILES_HS) \
        $(libHSgconf_a_HSCFILES_HS) \
        $(libHSgconf_a_GENERATEDSOURCES)
libHSgconf_a_HSFILES  = \
	$(filter %.hs,$(libHSgconf_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgconf_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgconf_a_HSFILES:.hs=.hi)

gconf_MOSTLYCLEANFILES  = $(am_libHSgconf_a_OBJECTS)
gconf_MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.hi)
gconf_MOSTLYCLEANFILES += \
	$(libHSgconf_a_CHSFILES:.chs=.chi)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.h)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.o)	\
	$(libHSgconf_a_CHSFILES:.chs=_stub.c)
gconf_CLEANFILES  = $(libHSgconf_a_BUILDSOURCES)
gconf_CLEANFILES += $(libHSgconf_a_CHSFILES_HS:.hs=.dep)
gconf_CLEANFILES += \
	$(libHSgconf_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSgconf_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSgconf_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgconf_a_CHSFILES_HS): $(libHSgconf_a_PRECOMP)
$(libHSgconf_a_CHSFILES:.chs=.dep) :	\
	$(libHSgconf_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgconf_a_CHSFILES:.chs=.dep) gconf/libHSgconf_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgconf_a_AR = $(srcdir)/mk/link-splitobjs.sh
gconf_CLEANFILES_SPLIT = $(libHSgconf_a_HSFILES:.hs=_split/)
else
libHSgconf_a_AR = $(AR) $(ARFLAGS)
endif

libHSgconf_a_DEPENDENCIES = HSgconf.o
pkglib_DATA += HSgconf.o
HSgconf.o : $(libHSgconf_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgconf_a_OBJECTS) $(libHSgconf_a_LIBADD)

if ENABLE_PROFILING
libHSgconf_a_DEPENDENCIES += libHSgconf_p.a
pkglib_DATA += libHSgconf_p.a
libHSgconf_p.a : $(libHSgconf_a_HSFILES:.hs=.p_o) \
		$(libHSgconf_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gconf/libHSgconf_a.p_deps
 endif

nobase_hi_DATA         += $(libHSgconf_a_HSFILES:.hs=.p_hi)
gconf_MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.p_hi)
gconf_MOSTLYCLEANFILES += $(libHSgconf_a_HSFILES:.hs=.p_o)
gconf_MOSTLYCLEANFILES += $(libHSgconf_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_GCONF

#
#	sourceview package
#
################################################################################

if ENABLE_SOURCEVIEW

sourceview_PKGNAME = libHSsourceview_a

libHSsourceview_a_NAME		= sourceview
libHSsourceview_a_CONFIG	= sourceview/sourceview.package.conf
libHSsourceview_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSsourceview_a_INTERNALDEPS	= glib gtk
libHSsourceview_a_HEADER 	= sourceview.h
libHSsourceview_a_PRECOMP	= sourceview/sourceview.precomp
libHSsourceview_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(SOURCEVIEW_LIBS)
libHSsourceview_a_HCFLAGS	= $(FFI)
libHSsourceview_a_CFLAGS	= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS))
libHSsourceview_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(SOURCEVIEW_CFLAGS)) -Isourceview

libHSsourceview_a_SOURCESDIRS	= sourceview

sourceview/libHSsourceview_a.deps	: $(libHSsourceview_a_HSFILES) | gtk/libHSgtk_a.deps

libHSsourceview_a_GENERATEDSOURCES =						\
	sourceview/Graphics/UI/Gtk/SourceView/Types.chs

nodist_libHSsourceview_a_SOURCES = $(libHSsourceview_a_GENERATEDSOURCES)

libHSsourceview_a_SOURCES =							\
	sourceview/Graphics/UI/Gtk/SourceView.hs				\
	sourceview/Graphics/UI/Gtk/SourceView/SourceBuffer.chs			\
	sourceview/Graphics/UI/Gtk/SourceView/SourceIter.chs			\
	sourceview/Graphics/UI/Gtk/SourceView/SourceLanguage.chs		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceLanguagesManager.chs	\
	sourceview/Graphics/UI/Gtk/SourceView/SourceMarker.chs			\
	sourceview/Graphics/UI/Gtk/SourceView/SourceStyleScheme.chs		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceTag.chs			\
        sourceview/Graphics/UI/Gtk/SourceView/SourceTagStyle.hsc		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceTagTable.chs		\
	sourceview/Graphics/UI/Gtk/SourceView/SourceView.chs

htmldoc_HSFILES_HIDDEN += $(libHSsourceview_a_GENERATEDSOURCES:.chs=.hs)

sourceview_Graphics_UI_Gtk_SourceView_Types_hs_HCFLAGS	= -fglasgow-exts
sourceview_Graphics_UI_Gtk_SourceView_SourceTagStyle_hs_HCFLAGS	= -fglasgow-exts

sourceview/Graphics/UI/Gtk/SourceView/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=sourceview --parentname=Graphics.UI.Gtk.Types \
        --modname=Graphics.UI.Gtk.SourceView.Types)

libHSsourceview_a_ALLSOURCES = $(libHSsourceview_a_SOURCES) $(nodist_libHSsourceview_a_SOURCES)

am_libHSsourceview_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsourceview_a_ALLSOURCES))))

libHSsourceview_a_HSPPFILES  = $(filter %.hs.pp, $(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_CHSPPFILES = $(filter %.chs.pp,$(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_CHSFILES = \
  $(filter %.chs,$(libHSsourceview_a_ALLSOURCES:.chs.pp=.chs))
libHSsourceview_a_CHSFILES_HS = $(libHSsourceview_a_CHSFILES:.chs=.hs)
libHSsourceview_a_HSCFILES = $(filter %.hsc, $(libHSsourceview_a_ALLSOURCES))
libHSsourceview_a_HSCFILES_HS = $(libHSsourceview_a_HSCFILES:.hsc=.hs)
libHSsourceview_a_BUILDSOURCES = \
	$(libHSsourceview_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSsourceview_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSsourceview_a_CHSFILES_HS) \
        $(libHSsourceview_a_HSCFILES_HS) \
        $(libHSsourceview_a_GENERATEDSOURCES)
libHSsourceview_a_HSFILES  = \
	$(filter %.hs,$(libHSsourceview_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsourceview_a_ALLSOURCES))

nobase_hi_DATA += $(libHSsourceview_a_HSFILES:.hs=.hi)

sourceview_MOSTLYCLEANFILES  = $(am_libHSsourceview_a_OBJECTS)
sourceview_MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.hi)
sourceview_MOSTLYCLEANFILES += $(libHSsourceview_a_CHSFILES:.chs=.chi)
sourceview_CLEANFILES  = $(libHSsourceview_a_BUILDSOURCES)
sourceview_CLEANFILES += $(libHSsourceview_a_CHSFILES_HS:.hs=.dep)
sourceview_CLEANFILES += \
	$(libHSsourceview_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSsourceview_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSsourceview_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSsourceview_a_CHSFILES_HS): $(libHSsourceview_a_PRECOMP)
$(libHSsourceview_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)		\
	$(libHSsourceview_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSsourceview_a_CHSFILES:.chs=.dep) sourceview/libHSsourceview_a.deps
 endif

if ENABLE_SPLITOBJS
libHSsourceview_a_AR = $(srcdir)/mk/link-splitobjs.sh
sourceview_CLEANFILES_SPLIT = $(libHSsourceview_a_HSFILES:.hs=_split/)
else
libHSsourceview_a_AR = $(AR) $(ARFLAGS)
endif

libHSsourceview_a_DEPENDENCIES = HSsourceview.o
pkglib_DATA += HSsourceview.o
HSsourceview.o : $(libHSsourceview_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSsourceview_a_OBJECTS) $(libHSsourceview_a_LIBADD)

if ENABLE_PROFILING
libHSsourceview_a_DEPENDENCIES += libHSsourceview_p.a
pkglib_DATA += libHSsourceview_p.a
libHSsourceview_p.a : $(libHSsourceview_a_HSFILES:.hs=.p_o) \
		$(libHSsourceview_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include sourceview/libHSsourceview_a.p_deps
 endif

nobase_hi_DATA              += $(libHSsourceview_a_HSFILES:.hs=.p_hi)
sourceview_MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.p_hi)
sourceview_MOSTLYCLEANFILES += $(libHSsourceview_a_HSFILES:.hs=.p_o)
sourceview_MOSTLYCLEANFILES += $(libHSsourceview_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_SOURCEVIEW

#
#	gtksourceview2 package
#
################################################################################

if ENABLE_GTKSOURCEVIEW2

gtksourceview2_PKGNAME = libHSgtksourceview2_a

libHSgtksourceview2_a_NAME		= gtksourceview2
libHSgtksourceview2_a_CONFIG	= gtksourceview2/gtksourceview2.package.conf
libHSgtksourceview2_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSgtksourceview2_a_INTERNALDEPS	= glib gtk
libHSgtksourceview2_a_HEADER 	= gtksourceview2.h
libHSgtksourceview2_a_PRECOMP	= gtksourceview2/gtksourceview2.precomp
libHSgtksourceview2_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(GTKSOURCEVIEW2_LIBS)
libHSgtksourceview2_a_HCFLAGS	= $(FFI)
libHSgtksourceview2_a_CFLAGS	= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTKSOURCEVIEW2_CFLAGS))
libHSgtksourceview2_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTKSOURCEVIEW2_CFLAGS)) -Igtksourceview2

libHSgtksourceview2_a_SOURCESDIRS	= gtksourceview2

gtksourceview2/libHSgtksourceview2_a.deps	: $(libHSgtksourceview2_a_HSFILES) | gtk/libHSgtk_a.deps

libHSgtksourceview2_a_GENERATEDSOURCES =					\
	gtksourceview2/Graphics/UI/Gtk/SourceView/Types.chs			\
	gtksourceview2/Graphics/UI/Gtk/SourceView/Signals.chs

nodist_libHSgtksourceview2_a_SOURCES = $(libHSgtksourceview2_a_GENERATEDSOURCES)

libHSgtksourceview2_a_SOURCES =							\
	gtksourceview2/Graphics/UI/Gtk/SourceView.hs				\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceBuffer.chs		\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceIter.chs		\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceLanguage.chs		\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceLanguageManager.chs.pp	\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceStyle.hs		\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceStyle/Internal.chs	\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceStyleScheme.chs		\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceStyleSchemeManager.chs	\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceView.chs

if HAVE_GTKSOURCEVIEW2_VERSION_2_2
libHSgtksourceview2_a_SOURCES +=						\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceMark.chs
endif

htmldoc_HSFILES_HIDDEN += $(libHSgtksourceview2_a_GENERATEDSOURCES:.chs=.hs)	\
	gtksourceview2/Graphics/UI/Gtk/SourceView/SourceStyle/Internal.hs

gtksourceview2_Graphics_UI_Gtk_SourceView_Types_hs_HCFLAGS		= -fglasgow-exts
gtksourceview2_Graphics_UI_Gtk_SourceView_SourceTagStyle_hs_HCFLAGS	= -fglasgow-exts

gtksourceview2/Graphics/UI/Gtk/SourceView/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ $(addprefix --tag=,$(GTKSOURCEVIEW2_CREATE_TYPES)) \
	--parentname=Graphics.UI.Gtk.Types \
        --modname=Graphics.UI.Gtk.SourceView.Types)

gtksourceview2/Graphics/UI/Gtk/SourceView/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/gtksourceview2/marshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/gtksourceview2/marshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	Graphics.UI.Gtk.SourceView.Signals)

libHSgtksourceview2_a_ALLSOURCES = $(libHSgtksourceview2_a_SOURCES) $(nodist_libHSgtksourceview2_a_SOURCES)

am_libHSgtksourceview2_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtksourceview2_a_ALLSOURCES))))

libHSgtksourceview2_a_HSPPFILES  = $(filter %.hs.pp, $(libHSgtksourceview2_a_ALLSOURCES))
libHSgtksourceview2_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgtksourceview2_a_ALLSOURCES))
libHSgtksourceview2_a_CHSFILES = \
  $(filter %.chs,$(libHSgtksourceview2_a_ALLSOURCES:.chs.pp=.chs))
libHSgtksourceview2_a_CHSFILES_HS = $(libHSgtksourceview2_a_CHSFILES:.chs=.hs)
libHSgtksourceview2_a_HSCFILES = $(filter %.hsc, $(libHSgtksourceview2_a_ALLSOURCES))
libHSgtksourceview2_a_HSCFILES_HS = $(libHSgtksourceview2_a_HSCFILES:.hsc=.hs)
libHSgtksourceview2_a_BUILDSOURCES = \
	$(libHSgtksourceview2_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSgtksourceview2_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtksourceview2_a_CHSFILES_HS) \
        $(libHSgtksourceview2_a_HSCFILES_HS) \
        $(libHSgtksourceview2_a_GENERATEDSOURCES)
libHSgtksourceview2_a_HSFILES  = \
	$(filter %.hs,$(libHSgtksourceview2_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtksourceview2_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgtksourceview2_a_HSFILES:.hs=.hi)

gtksourceview2_MOSTLYCLEANFILES  = $(am_libHSgtksourceview2_a_OBJECTS)
gtksourceview2_MOSTLYCLEANFILES += $(libHSgtksourceview2_a_HSFILES:.hs=.hi)
gtksourceview2_MOSTLYCLEANFILES += $(libHSgtksourceview2_a_CHSFILES:.chs=.chi)
gtksourceview2_CLEANFILES  = $(libHSgtksourceview2_a_BUILDSOURCES)
gtksourceview2_CLEANFILES += $(libHSgtksourceview2_a_CHSFILES_HS:.hs=.dep)
gtksourceview2_CLEANFILES += \
	$(libHSgtksourceview2_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSgtksourceview2_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSgtksourceview2_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgtksourceview2_a_CHSFILES_HS): $(libHSgtksourceview2_a_PRECOMP)
$(libHSgtksourceview2_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)		\
	$(libHSgtksourceview2_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgtksourceview2_a_CHSFILES:.chs=.dep) gtksourceview2/libHSgtksourceview2_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgtksourceview2_a_AR = $(srcdir)/mk/link-splitobjs.sh
gtksourceview2_CLEANFILES_SPLIT = $(libHSgtksourceview2_a_HSFILES:.hs=_split/)
else
libHSgtksourceview2_a_AR = $(AR) $(ARFLAGS)
endif

libHSgtksourceview2_a_DEPENDENCIES = HSgtksourceview2.o
pkglib_DATA += HSgtksourceview2.o
HSgtksourceview2.o : $(libHSgtksourceview2_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgtksourceview2_a_OBJECTS) $(libHSgtksourceview2_a_LIBADD)

if ENABLE_PROFILING
libHSgtksourceview2_a_DEPENDENCIES += libHSgtksourceview2_p.a
pkglib_DATA += libHSgtksourceview2_p.a
libHSgtksourceview2_p.a : $(libHSgtksourceview2_a_HSFILES:.hs=.p_o) \
		$(libHSgtksourceview2_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gtksourceview2/libHSgtksourceview2_a.p_deps
 endif

nobase_hi_DATA              += $(libHSgtksourceview2_a_HSFILES:.hs=.p_hi)
gtksourceview2_MOSTLYCLEANFILES += $(libHSgtksourceview2_a_HSFILES:.hs=.p_hi)
gtksourceview2_MOSTLYCLEANFILES += $(libHSgtksourceview2_a_HSFILES:.hs=.p_o)
gtksourceview2_MOSTLYCLEANFILES += $(libHSgtksourceview2_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_GTKSOURCEVIEW2

#
#	mozembed package
#
################################################################################

if ENABLE_MOZEMBED

mozembed_PKGNAME = libHSmozembed_a

libHSmozembed_a_NAME		= mozembed
libHSmozembed_a_CONFIG		= mozembed/mozembed.package.conf
libHSmozembed_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSmozembed_a_INTERNALDEPS	= glib gtk
libHSmozembed_a_HEADER 		= gtkmozembed.h
libHSmozembed_a_PRECOMP		= mozembed/mozembed.precomp
libHSmozembed_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(MOZEMBED_LIBS)
libHSmozembed_a_HCFLAGS		= $(FFI)
libHSmozembed_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))
libHSmozembed_a_CPPFLAGS	= $(filter -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(MOZEMBED_CFLAGS))

libHSmozembed_a_SOURCESDIRS	= mozembed

mozembed/libHSmozembed_a.deps	: $(libHSmozembed_a_HSFILES) | gtk/libHSgtk_a.deps

libHSmozembed_a_GENERATEDSOURCES =			\
	mozembed/Graphics/UI/Gtk/MozEmbed/Types.chs

nodist_libHSmozembed_a_SOURCES = $(libHSmozembed_a_GENERATEDSOURCES)

libHSmozembed_a_SOURCES =				\
	mozembed/Graphics/UI/Gtk/MozEmbed.chs.pp

htmldoc_HSFILES_HIDDEN += $(libHSmozembed_a_GENERATEDSOURCES:.chs=.hs)

mozembed_Graphics_UI_Gtk_MozEmbed_Types_hs_HCFLAGS = -fglasgow-exts

mozembed/Graphics/UI/Gtk/MozEmbed/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=mozembed --parentname=Graphics.UI.Gtk.Types \
	--modname=Graphics.UI.Gtk.MozEmbed.Types)

libHSmozembed_a_ALLSOURCES = $(libHSmozembed_a_SOURCES) $(nodist_libHSmozembed_a_SOURCES)

am_libHSmozembed_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSmozembed_a_ALLSOURCES))))

libHSmozembed_a_HSPPFILES  = $(filter %.hs.pp, $(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_CHSPPFILES = $(filter %.chs.pp,$(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_CHSFILES = $(filter %.chs,$(libHSmozembed_a_ALLSOURCES:.chs.pp=.chs))
libHSmozembed_a_CHSFILES_HS = $(libHSmozembed_a_CHSFILES:.chs=.hs)
libHSmozembed_a_HSCFILES = $(filter %.hsc, $(libHSmozembed_a_ALLSOURCES))
libHSmozembed_a_HSCFILES_HS = $(libHSmozembed_a_HSCFILES:.hsc=.hs)
libHSmozembed_a_BUILDSOURCES = \
	$(libHSmozembed_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSmozembed_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSmozembed_a_CHSFILES_HS) \
        $(libHSmozembed_a_HSCFILES_HS) \
        $(libHSmozembed_a_GENERATEDSOURCES)
libHSmozembed_a_HSFILES  = \
	$(filter %.hs,$(libHSmozembed_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSmozembed_a_ALLSOURCES))

nobase_hi_DATA += $(libHSmozembed_a_HSFILES:.hs=.hi)

mozembed_MOSTLYCLEANFILES  = $(am_libHSmozembed_a_OBJECTS)
mozembed_MOSTLYCLEANFILES += $(libHSmozembed_a_HSFILES:.hs=.hi)
mozembed_MOSTLYCLEANFILES += $(libHSmozembed_a_CHSFILES:.chs=.chi)
mozembed_CLEANFILES  = $(libHSmozembed_a_BUILDSOURCES)
mozembed_CLEANFILES += $(libHSmozembed_a_CHSFILES_HS:.hs=.dep)
mozembed_CLEANFILES += \
	$(libHSmozembed_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSmozembed_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSmozembed_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSmozembed_a_CHSFILES_HS): $(libHSmozembed_a_PRECOMP)
$(libHSmozembed_a_CHSFILES:.chs=.dep) : \
	$(libHSgtk_a_GENERATEDSOURCES)	\
	$(libHSmozembed_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSmozembed_a_CHSFILES:.chs=.dep) mozembed/libHSmozembed_a.deps
 endif

if ENABLE_SPLITOBJS
libHSmozembed_a_AR = $(srcdir)/mk/link-splitobjs.sh
mozembed_CLEANFILES_SPLIT = $(libHSmozembed_a_HSFILES:.hs=_split/)
else
libHSmozembed_a_AR = $(AR) $(ARFLAGS)
endif

libHSmozembed_a_DEPENDENCIES = HSmozembed.o
pkglib_DATA += HSmozembed.o
HSmozembed.o : $(libHSmozembed_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSmozembed_a_OBJECTS) $(libHSmozembed_a_LIBADD)

if ENABLE_PROFILING
libHSmozembed_a_DEPENDENCIES += libHSmozembed_p.a
pkglib_DATA += libHSmozembed_p.a
libHSmozembed_p.a : $(libHSmozembed_a_HSFILES:.hs=.p_o) \
		$(libHSmozembed_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include mozembed/libHSmozembed_a.p_deps
 endif

nobase_hi_DATA            += $(libHSmozembed_a_HSFILES:.hs=.p_hi)
mozembed_MOSTLYCLEANFILES += $(libHSmozembed_a_HSFILES:.hs=.p_hi)
mozembed_MOSTLYCLEANFILES += $(libHSmozembed_a_HSFILES:.hs=.p_o)
mozembed_MOSTLYCLEANFILES += $(libHSmozembed_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_MOZEMBED

#
#	cairo package
#
################################################################################

if ENABLE_CAIRO

cairo_PKGNAME = libHScairo_a

libHScairo_a_NAME		= cairo
libHScairo_a_CONFIG		= cairo/cairo.package.conf
libHScairo_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION) mtl-$(PKG_MTL_VERSION)
if HAVE_SPLIT_BASE
libHScairo_a_EXTERNALDEPS	+= bytestring-$(PKG_BYTESTRING_VERSION)
endif
libHScairo_a_INTERNALDEPS	= glib
libHScairo_a_HEADER 		= cairo/cairo-gtk2hs.h
libHScairo_a_PRECOMP		= cairo/cairo.precomp
libHScairo_a_LIBS		= $(CAIRO_LIBS)
libHScairo_a_HCFLAGS		= $(FFI)
libHScairo_a_CFLAGS		= $(filter-out -I% -D%,$(CAIRO_CFLAGS))
libHScairo_a_CPPFLAGS		= $(filter -I% -D%,$(CAIRO_CFLAGS))

libHScairo_a_SOURCESDIRS	= cairo

cairo/libHScairo_a.deps: $(libHScairo_a_HSFILES) | glib/libHSglib_a.deps

libHScairo_a_GENERATEDSOURCES =

nodist_libHScairo_a_SOURCES = $(libHScairo_a_GENERATEDSOURCES)

libHScairo_a_SOURCES =							\
	cairo/Graphics/Rendering/Cairo.hs.pp				\
	cairo/Graphics/Rendering/Cairo/Matrix.chs                       \
	cairo/Graphics/Rendering/Cairo/Types.chs                        \
	cairo/Graphics/Rendering/Cairo/Internal.hs			\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Cairo.chs       \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Paths.chs       \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Patterns.chs    \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Text.chs        \
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Transformations.chs \
	cairo/Graphics/Rendering/Cairo/Internal/Fonts/FontOptions.chs   \
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Image.chs.pp	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PDF.chs.pp	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PNG.chs.pp	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PS.chs.pp	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/SVG.chs.pp	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Surface.chs    \
	cairo/Graphics/Rendering/Cairo/Internal/Utilities.chs

cairo_Graphics_Rendering_Cairo_Internal_hs_HCFLAGS = -fglasgow-exts

htmldoc_HSFILES_HIDDEN += 							\
	cairo/Graphics/Rendering/Cairo/Types.hs
htmldoc_HSFILES_EXCLUDE =							\
	cairo/Graphics/Rendering/Cairo/Internal.hs			\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Cairo.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Paths.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Patterns.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Text.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Drawing/Transformations.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Fonts/FontOptions.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Image.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PDF.hs         \
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PNG.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/PS.hs          \
	cairo/Graphics/Rendering/Cairo/Internal/Surfaces/Surface.hs	\
	cairo/Graphics/Rendering/Cairo/Internal/Utilities.hs

libHScairo_a_ALLSOURCES = $(libHScairo_a_SOURCES) $(nodist_libHScairo_a_SOURCES)

am_libHScairo_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHScairo_a_ALLSOURCES))))

libHScairo_a_HSPPFILES  = $(filter %.hs.pp, $(libHScairo_a_ALLSOURCES))
libHScairo_a_CHSPPFILES = $(filter %.chs.pp,$(libHScairo_a_ALLSOURCES))
libHScairo_a_CHSFILES = \
  $(filter %.chs,$(libHScairo_a_ALLSOURCES:.chs.pp=.chs))
libHScairo_a_CHSFILES_HS = $(libHScairo_a_CHSFILES:.chs=.hs)
libHScairo_a_HSCFILES = $(filter %.hsc, $(libHScairo_a_ALLSOURCES))
libHScairo_a_HSCFILES_HS = $(libHScairo_a_HSCFILES:.hsc=.hs)
libHScairo_a_BUILDSOURCES = \
	$(libHScairo_a_HSPPFILES:.hs.pp=.hs) \
	$(libHScairo_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHScairo_a_CHSFILES_HS) \
        $(libHScairo_a_HSCFILES_HS) \
        $(libHScairo_a_GENERATEDSOURCES)
libHScairo_a_HSFILES  = \
	$(filter %.hs,$(libHScairo_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHScairo_a_ALLSOURCES))

nobase_hi_DATA += $(libHScairo_a_HSFILES:.hs=.hi)

cairo_MOSTLYCLEANFILES  = $(am_libHScairo_a_OBJECTS)
cairo_MOSTLYCLEANFILES += $(libHScairo_a_HSFILES:.hs=.hi)
cairo_MOSTLYCLEANFILES += $(libHScairo_a_CHSFILES:.chs=.chi)
cairo_CLEANFILES  = $(libHScairo_a_BUILDSOURCES)
cairo_CLEANFILES += $(libHScairo_a_CHSFILES_HS:.hs=.dep)
cairo_CLEANFILES += \
	$(libHScairo_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHScairo_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHScairo_a_HSCFILES:.hsc=_hsc_make.o)

$(libHScairo_a_CHSFILES_HS): $(libHScairo_a_PRECOMP)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHScairo_a_CHSFILES:.chs=.dep) cairo/libHScairo_a.deps
 endif

if ENABLE_SPLITOBJS
libHScairo_a_AR = $(srcdir)/mk/link-splitobjs.sh
cairo_CLEANFILES_SPLIT = $(libHScairo_a_HSFILES:.hs=_split/)
else
libHScairo_a_AR = $(AR) $(ARFLAGS)
endif

libHScairo_a_DEPENDENCIES = HScairo.o
pkglib_DATA += HScairo.o
HScairo.o : $(libHScairo_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHScairo_a_OBJECTS) $(libHScairo_a_LIBADD)

if ENABLE_PROFILING
libHScairo_a_DEPENDENCIES += libHScairo_p.a
pkglib_DATA += libHScairo_p.a
libHScairo_p.a : $(libHScairo_a_HSFILES:.hs=.p_o) \
		$(libHScairo_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include cairo/libHScairo_a.p_deps
 endif

nobase_hi_DATA         += $(libHScairo_a_HSFILES:.hs=.p_hi)
cairo_MOSTLYCLEANFILES += $(libHScairo_a_HSFILES:.hs=.p_hi)
cairo_MOSTLYCLEANFILES += $(libHScairo_a_HSFILES:.hs=.p_o)
cairo_MOSTLYCLEANFILES += $(libHScairo_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_CAIRO

#
#	svgcairo package
#
################################################################################

if ENABLE_SVGCAIRO

svgcairo_PKGNAME = libHSsvgcairo_a

libHSsvgcairo_a_NAME		= svgcairo
libHSsvgcairo_a_CONFIG		= svgcairo/svgcairo.package.conf
libHSsvgcairo_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION) mtl-$(PKG_MTL_VERSION)
libHSsvgcairo_a_INTERNALDEPS	= glib cairo
libHSsvgcairo_a_HEADER 		= svgcairo/svgcairo.h
libHSsvgcairo_a_PRECOMP		= svgcairo/svgcairo.precomp
libHSsvgcairo_a_LIBS		= $(SVGCAIRO_LIBS)
libHSsvgcairo_a_HCFLAGS		= $(FFI)
libHSsvgcairo_a_CFLAGS		= $(filter-out -I% -D%,$(CAIRO_CFLAGS) $(SVGCAIRO_CFLAGS))
libHSsvgcairo_a_CPPFLAGS	= $(filter -I% -D%,$(CAIRO_CFLAGS) $(SVGCAIRO_CFLAGS))

libHSsvgcairo_a_SOURCESDIRS	= svgcairo

svgcairo/libHSsvgcairo_a.deps: $(libHSsvgcairo_a_HSFILES) | cairo/libHScairo_a.deps

libHSsvgcairo_a_GENERATEDSOURCES =

nodist_libHSsvgcairo_a_SOURCES = $(libHSsvgcairo_a_GENERATEDSOURCES)

libHSsvgcairo_a_SOURCES =							\
	svgcairo/Graphics/Rendering/Cairo/SVG.chs.pp

libHSsvgcairo_a_ALLSOURCES = $(libHSsvgcairo_a_SOURCES) $(nodist_libHSsvgcairo_a_SOURCES)

am_libHSsvgcairo_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsvgcairo_a_ALLSOURCES))))

libHSsvgcairo_a_HSPPFILES  = $(filter %.hs.pp, $(libHSsvgcairo_a_ALLSOURCES))
libHSsvgcairo_a_CHSPPFILES = $(filter %.chs.pp,$(libHSsvgcairo_a_ALLSOURCES))
libHSsvgcairo_a_CHSFILES = \
  $(filter %.chs,$(libHSsvgcairo_a_ALLSOURCES:.chs.pp=.chs))
libHSsvgcairo_a_CHSFILES_HS = $(libHSsvgcairo_a_CHSFILES:.chs=.hs)
libHSsvgcairo_a_HSCFILES = $(filter %.hsc, $(libHSsvgcairo_a_ALLSOURCES))
libHSsvgcairo_a_HSCFILES_HS = $(libHSsvgcairo_a_HSCFILES:.hsc=.hs)
libHSsvgcairo_a_BUILDSOURCES = \
	$(libHSsvgcairo_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSsvgcairo_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSsvgcairo_a_CHSFILES_HS) \
        $(libHSsvgcairo_a_HSCFILES_HS) \
        $(libHSsvgcairo_a_GENERATEDSOURCES)
libHSsvgcairo_a_HSFILES  = \
	$(filter %.hs,$(libHSsvgcairo_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsvgcairo_a_ALLSOURCES))

nobase_hi_DATA += $(libHSsvgcairo_a_HSFILES:.hs=.hi)

svgcairo_MOSTLYCLEANFILES  = $(am_libHSsvgcairo_a_OBJECTS)
svgcairo_MOSTLYCLEANFILES += $(libHSsvgcairo_a_HSFILES:.hs=.hi)
svgcairo_MOSTLYCLEANFILES += $(libHSsvgcairo_a_CHSFILES:.chs=.chi)
svgcairo_CLEANFILES  = $(libHSsvgcairo_a_BUILDSOURCES)
svgcairo_CLEANFILES += $(libHSsvgcairo_a_CHSFILES_HS:.hs=.dep)
svgcairo_CLEANFILES += \
	$(libHSsvgcairo_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSsvgcairo_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSsvgcairo_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSsvgcairo_a_CHSFILES_HS): $(libHSsvgcairo_a_PRECOMP)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSsvgcairo_a_CHSFILES:.chs=.dep) svgcairo/libHSsvgcairo_a.deps
 endif

if ENABLE_SPLITOBJS
libHSsvgcairo_a_AR = $(srcdir)/mk/link-splitobjs.sh
svgcairo_CLEANFILES_SPLIT = $(libHSsvgcairo_a_HSFILES:.hs=_split/)
else
libHSsvgcairo_a_AR = $(AR) $(ARFLAGS)
endif

libHSsvgcairo_a_DEPENDENCIES = HSsvgcairo.o
pkglib_DATA += HSsvgcairo.o
HSsvgcairo.o : $(libHSsvgcairo_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSsvgcairo_a_OBJECTS) $(libHSsvgcairo_a_LIBADD)

if ENABLE_PROFILING
libHSsvgcairo_a_DEPENDENCIES += libHSsvgcairo_p.a
pkglib_DATA += libHSsvgcairo_p.a
libHSsvgcairo_p.a : $(libHSsvgcairo_a_HSFILES:.hs=.p_o) \
		$(libHSsvgcairo_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include svgcairo/libHSsvgcairo_a.p_deps
 endif

nobase_hi_DATA            += $(libHSsvgcairo_a_HSFILES:.hs=.p_hi)
svgcairo_MOSTLYCLEANFILES += $(libHSsvgcairo_a_HSFILES:.hs=.p_hi)
svgcairo_MOSTLYCLEANFILES += $(libHSsvgcairo_a_HSFILES:.hs=.p_o)
svgcairo_MOSTLYCLEANFILES += $(libHSsvgcairo_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_SVGCAIRO

#
#	gtkglext package
#
################################################################################

if ENABLE_GTKGLEXT

gtkglext_PKGNAME = libHSgtkglext_a

libHSgtkglext_a_NAME		= gtkglext
libHSgtkglext_a_CONFIG		= gtkglext/gtkglext.package.conf
libHSgtkglext_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
libHSgtkglext_a_INTERNALDEPS	= glib gtk
libHSgtkglext_a_HEADER 		= gtk/gtkgl.h
libHSgtkglext_a_PRECOMP		= gtkglext/gtkglext.precomp
libHSgtkglext_a_LIBS		= $(GLIB_LIBS) $(GTK_LIBS) $(GTKGLEXT_LIBS)
libHSgtkglext_a_HCFLAGS		= $(FFI)
libHSgtkglext_a_CFLAGS		= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTKGLEXT_CFLAGS))
libHSgtkglext_a_CPPFLAGS	= $(filter     -I% -D%,$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTKGLEXT_CFLAGS))

libHSgtkglext_a_SOURCESDIRS	= gtkglext

gtkglext/libHSgtkglext_a.deps	: $(libHSgtkglext_a_HSFILES) | gtk/libHSgtk_a.deps

libHSgtkglext_a_GENERATEDSOURCES =					\
	gtkglext/Graphics/UI/Gtk/OpenGL/Types.chs

nodist_libHSgtkglext_a_SOURCES = $(libHSgtkglext_a_GENERATEDSOURCES)

libHSgtkglext_a_SOURCES =						\
	gtkglext/Graphics/UI/Gtk/OpenGL.hs				\
	gtkglext/Graphics/UI/Gtk/OpenGL/Config.chs.pp			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Context.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Drawable.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Pixmap.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/Window.chs			\
	gtkglext/Graphics/UI/Gtk/OpenGL/General.chs.pp			\
	gtkglext/Graphics/UI/Gtk/OpenGL/DrawingArea.chs

htmldoc_HSFILES_HIDDEN += $(libHSgtkglext_a_GENERATEDSOURCES:.chs=.hs)

gtkglext_Graphics_UI_Gtk_OpenGL_Types_hs_HCFLAGS	= -fglasgow-exts

gtkglext/Graphics/UI/Gtk/OpenGL/Types.chs :	\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gtkglext --parentname=Graphics.UI.Gtk.Types \
        --modname=Graphics.UI.Gtk.OpenGL.Types)

libHSgtkglext_a_ALLSOURCES = $(libHSgtkglext_a_SOURCES) $(nodist_libHSgtkglext_a_SOURCES)

am_libHSgtkglext_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgtkglext_a_ALLSOURCES))))

libHSgtkglext_a_HSPPFILES  = $(filter %.hs.pp, $(libHSgtkglext_a_ALLSOURCES))
libHSgtkglext_a_CHSPPFILES = $(filter %.chs.pp,$(libHSgtkglext_a_ALLSOURCES))
libHSgtkglext_a_CHSFILES = \
  $(filter %.chs,$(libHSgtkglext_a_ALLSOURCES:.chs.pp=.chs))
libHSgtkglext_a_CHSFILES_HS = $(libHSgtkglext_a_CHSFILES:.chs=.hs)
libHSgtkglext_a_HSCFILES = $(filter %.hsc, $(libHSgtkglext_a_ALLSOURCES))
libHSgtkglext_a_HSCFILES_HS = $(libHSgtkglext_a_HSCFILES:.hsc=.hs)
libHSgtkglext_a_BUILDSOURCES = \
	$(libHSgtkglext_a_HSPPFILES:.hs.pp=.hs) \
	$(libHSgtkglext_a_CHSPPFILES:.chs.pp=.chs) \
	$(libHSgtkglext_a_CHSFILES_HS) \
        $(libHSgtkglext_a_HSCFILES_HS) \
        $(libHSgtkglext_a_GENERATEDSOURCES)
libHSgtkglext_a_HSFILES  = \
	$(filter %.hs,$(libHSgtkglext_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSgtkglext_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgtkglext_a_HSFILES:.hs=.hi)

gtkglext_MOSTLYCLEANFILES  = $(am_libHSgtkglext_a_OBJECTS)
gtkglext_MOSTLYCLEANFILES += $(libHSgtkglext_a_HSFILES:.hs=.hi)
gtkglext_MOSTLYCLEANFILES += $(libHSgtkglext_a_CHSFILES:.chs=.chi)
gtkglext_CLEANFILES  = $(libHSgtkglext_a_BUILDSOURCES)
gtkglext_CLEANFILES += $(libHSgtkglext_a_CHSFILES_HS:.hs=.dep)
gtkglext_CLEANFILES += \
	$(libHSgtkglext_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSgtkglext_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSgtkglext_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgtkglext_a_CHSFILES_HS): $(libHSgtkglext_a_PRECOMP)
$(libHSgtkglext_a_CHSFILES:.chs=.dep) :	\
	$(libHSgtk_a_GENERATEDSOURCES)		\
	$(libHSgtkglext_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgtkglext_a_CHSFILES:.chs=.dep) gtkglext/libHSgtkglext_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgtkglext_a_AR = $(srcdir)/mk/link-splitobjs.sh
gtkglext_CLEANFILES_SPLIT = $(libHSgtkglext_a_HSFILES:.hs=_split/)
else
libHSgtkglext_a_AR = $(AR) $(ARFLAGS)
endif

libHSgtkglext_a_DEPENDENCIES = HSgtkglext.o
pkglib_DATA += HSgtkglext.o
HSgtkglext.o : $(libHSgtkglext_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgtkglext_a_OBJECTS) $(libHSgtkglext_a_LIBADD)

if ENABLE_PROFILING
libHSgtkglext_a_DEPENDENCIES += libHSgtkglext_p.a
pkglib_DATA += libHSgtkglext_p.a
libHSgtkglext_p.a : $(libHSgtkglext_a_HSFILES:.hs=.p_o) \
		$(libHSgtkglext_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gtkglext/libHSgtkglext_a.p_deps
 endif

nobase_hi_DATA            += $(libHSgtkglext_a_HSFILES:.hs=.p_hi)
gtkglext_MOSTLYCLEANFILES += $(libHSgtkglext_a_HSFILES:.hs=.p_hi)
gtkglext_MOSTLYCLEANFILES += $(libHSgtkglext_a_HSFILES:.hs=.p_o)
gtkglext_MOSTLYCLEANFILES += $(libHSgtkglext_a_LIBADD:.o=.p_o)
endif

endif #ENABLE_GTKGLEXT

#
#	soegtk package
#
################################################################################

if ENABLE_GTK

soegtk_PKGNAME = libHSsoegtk_a

libHSsoegtk_a_NAME		= soegtk
libHSsoegtk_a_CONFIG		= soegtk/soegtk.package.conf
libHSsoegtk_a_EXTERNALDEPS	= base-$(PKG_BASE_VERSION)
if HAVE_SPLIT_BASE
libHSsoegtk_a_EXTERNALDEPS	+= old-time-$(PKG_OLDTIME_VERSION)
endif
libHSsoegtk_a_INTERNALDEPS	= glib gtk
libHSsoegtk_a_HEADER		= $(libHSgtk_a_HEADER)
libHSsoegtk_a_LIBS		= 
libHSsoegtk_a_HCFLAGS		=
libHSsoegtk_a_CFLAGS 		= $(libHSgtk_a_CFLAGS)
libHSsoegtk_a_CPPFLAGS 		= $(libHSgtk_a_CPPFLAGS)

libHSsoegtk_a_SOURCESDIRS	= soegtk

soegtk/libHSsoegtk_a.deps	: $(libHSsoegtk_a_HSFILES) | gtk/libHSgtk_a.deps

if ENABLE_CAIRO
libHSsoegtk_a_EXTERNALDEPS += mtl-$(PKG_MTL_VERSION)
libHSsoegtk_a_INTERNALDEPS += cairo
gtk/libHSsoegtk_a.deps	: cairo/libHScairo_a.deps
endif

libHSsoegtk_a_SOURCES =			\
	soegtk/Graphics/SOE/Gtk.hs.pp

am_libHSsoegtk_a_OBJECTS = \
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSsoegtk_a_SOURCES))))

libHSsoegtk_a_HSPPFILES    = $(filter %.hs.pp, $(libHSsoegtk_a_SOURCES))
libHSsoegtk_a_BUILDSOURCES = \
	$(libHSsoegtk_a_HSPPFILES:.hs.pp=.hs)
libHSsoegtk_a_HSFILES  = \
	$(filter %.hs,$(libHSsoegtk_a_BUILDSOURCES)) \
	$(filter %.hs,$(libHSsoegtk_a_ALLSOURCES))

nobase_hi_DATA += $(libHSsoegtk_a_HSFILES:.hs=.hi)

soegtk_MOSTLYCLEANFILES  = $(am_libHSsoegtk_a_OBJECTS)
soegtk_MOSTLYCLEANFILES += $(libHSsoegtk_a_HSFILES:.hs=.hi)
soegtk_MOSTLYCLEANFILES += $(libHSsoegtk_a_BUILDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include soegtk/libHSsoegtk_a.deps
 endif

if ENABLE_SPLITOBJS
libHSsoegtk_a_AR = $(srcdir)/mk/link-splitobjs.sh
soegtk_CLEANFILES_SPLIT = $(libHSsoegtk_a_HSFILES:.hs=_split/)
else
libHSsoegtk_a_AR = $(AR) $(ARFLAGS)
endif

libHSsoegtk_a_DEPENDENCIES = HSsoegtk.o
pkglib_DATA += HSsoegtk.o
HSsoegtk.o : $(libHSsoegtk_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSsoegtk_a_OBJECTS) $(libHSsoegtk_a_LIBADD)

if ENABLE_PROFILING
libHSsoegtk_a_DEPENDENCIES += libHSsoegtk_p.a
pkglib_DATA += libHSsoegtk_p.a
libHSsoegtk_p.a : $(libHSsoegtk_a_HSFILES:.hs=.p_o) \
		$(libHSsoegtk_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include soegtk/libHSsoegtk_a.p_deps
 endif

nobase_hi_DATA          += $(libHSsoegtk_a_HSFILES:.hs=.p_hi)
soegtk_MOSTLYCLEANFILES += $(libHSsoegtk_a_HSFILES:.hs=.p_hi)
soegtk_MOSTLYCLEANFILES += $(libHSsoegtk_a_HSFILES:.hs=.p_o)
soegtk_MOSTLYCLEANFILES += $(libHSsoegtk_a_LIBADD:.o=.p_o)
endif

endif

#
#      gnomevfs package
#
################################################################################

if ENABLE_GNOMEVFS

gnomevfs_PKGNAME = libHSgnomevfs_a

libHSgnomevfs_a_NAME			= gnomevfs
libHSgnomevfs_a_CONFIG			= gnomevfs/gnomevfs.package.conf
libHSgnomevfs_a_EXTERNALDEPS		= base-$(PKG_BASE_VERSION) mtl-$(PKG_MTL_VERSION)
if HAVE_SPLIT_BASE
libHSgnomevfs_a_EXTERNALDEPS           += bytestring-$(PKG_BYTESTRING_VERSION)
endif
libHSgnomevfs_a_INTERNALDEPS		= glib
libHSgnomevfs_a_PRECOMP			= gnomevfs/gnomevfs.precomp
if HAVE_GNOME_VFS_VERSION_2_14
libHSgnomevfs_a_HEADER			= gnomevfs/hsgnomevfs-2.14.h
$(libHSgnomevfs_a_PRECOMP): $(libHSgnomevfs_a_HEADER)
else
libHSgnomevfs_a_HEADER			= libgnomevfs/gnome-vfs.h
endif
libHSgnomevfs_a_LIBS			= $(GLIB_LIBS) $(GNOMEVFS_LIBS) $(GCONF_LIBS)
libHSgnomevfs_a_HCFLAGS			= $(FFI)
libHSgnomevfs_a_CFLAGS			= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GNOMEVFS_CFLAGS))
libHSgnomevfs_a_CPPFLAGS		= $(filter     -I% -D%,$(GLIB_CFLAGS) $(GNOMEVFS_CFLAGS)) -I.

libHSgnomevfs_a_SOURCESDIRS		= gnomevfs

libHSgnomevfs_a_LIBADD			=		\
	gnomevfs/System/Gnome/VFS/Directory_stub.o      \
	gnomevfs/System/Gnome/VFS/Marshal_stub.o       \
	gnomevfs/System/Gnome/VFS/Monitor_stub.o	\
	gnomevfs/System/Gnome/VFS/Xfer_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgnomevfs_a_LIBADD += gnomevfs/System/Gnome/VFS/Signals_stub.o
endif

gnomevfs/libHSgnomevfs_a.deps	: $(libHSgnomevfs_a_HSFILES) glib/libHSglib_a.deps

libHSgnomevfs_a_GENERATEDSOURCES	=		\
	gnomevfs/System/Gnome/VFS/Signals.chs		\
	gnomevfs/System/Gnome/VFS/Hierarchy.chs

nodist_libHSgnomevfs_a_SOURCES		= $(libHSgnomevfs_a_GENERATEDSOURCES)

libHSgnomevfs_a_SOURCES			=               \
	gnomevfs/System/Gnome/VFS/Constants.hsc		\
	gnomevfs/System/Gnome/VFS/Types.chs.pp          \
	gnomevfs/System/Gnome/VFS/Error.hs.pp		\
	gnomevfs/System/Gnome/VFS/Marshal.chs		\
	gnomevfs/System/Gnome/VFS/Init.chs              \
	gnomevfs/System/Gnome/VFS/hsfileinfo.c          \
	gnomevfs/System/Gnome/VFS/FileInfo.chs.pp       \
	gnomevfs/System/Gnome/VFS/Monitor.chs           \
	gnomevfs/System/Gnome/VFS/Ops.chs.pp            \
	gnomevfs/System/Gnome/VFS/Directory.chs         \
	gnomevfs/System/Gnome/VFS/URI.chs.pp            \
	gnomevfs/System/Gnome/VFS/Util.chs              \
	gnomevfs/System/Gnome/VFS/Xfer.chs.pp           \
	gnomevfs/System/Gnome/VFS/Cancellation.chs      \
	gnomevfs/System/Gnome/VFS/Volume.chs.pp         \
	gnomevfs/System/Gnome/VFS/Drive.chs.pp          \
	gnomevfs/System/Gnome/VFS/VolumeMonitor.chs     \
	gnomevfs/System/Gnome/VFS.hs.pp

if HAVE_GNOME_VFS_VERSION_2_14
libHSgnomevfs_a_SOURCES += \
	gnomevfs/System/Gnome/VFS/MIME.chs
endif

htmldoc_HSFILES_HIDDEN				+=	\
	$(libHSgnomevfs_a_GENERATEDSOURCES:.chs=.hs)	\
	gnomevfs/System/Gnome/VFS/Types.hs		\
	gnomevfs/System/Gnome/VFS/Constants.hs		\
	gnomevfs/System/Gnome/VFS/Marshal.hs

gnomevfs_System_Gnome_VFS_FileInfo_hs_HCFLAGS	 = '-\#include"hsfileinfo.h"'
gnomevfs_System_Gnome_VFS_Types_hs_HCFLAGS	 = -fglasgow-exts
gnomevfs_System_Gnome_VFS_Directory_hs_HCFLAGS	 = -fglasgow-exts

libHSgnomevfs_a_ALLSOURCES			 = $(libHSgnomevfs_a_SOURCES) $(nodist_libHSgnomevfs_a_SOURCES)

gnomevfs/System/Gnome/VFS/Hierarchy.chs :			\
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT)  \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGen/TypeGenerator$(EXEEXT) \
	$(srcdir)/tools/hierarchyGen/hierarchy.list \
	$(srcdir)/tools/hierarchyGen/Hierarchy.chs.template \
	$@ --tag=gnomevfs --lib=gnomevfs --prefix=gnome_vfs \
	--modname=System.Gnome.VFS.Hierarchy --parentname=System.Glib.GObject)

gnomevfs/System/Gnome/VFS/Signals.chs : \
	$(srcdir)/tools/callbackGen/Signal.chs.template \
	$(srcdir)/gnomevfs/marshal.list \
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) \
	$(srcdir)/gnomevfs/marshal.list \
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ \
	System.Gnome.VFS.Signals)

am_libHSgnomevfs_a_OBJECTS			 =	\
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgnomevfs_a_ALLSOURCES))))

libHSgnomevfs_a_HSPPFILES	= $(filter %.hs.pp, $(libHSgnomevfs_a_ALLSOURCES))
libHSgnomevfs_a_CHSPPFILES	= $(filter %.chs.pp,$(libHSgnomevfs_a_ALLSOURCES))
libHSgnomevfs_a_CHSFILES	=			\
	$(filter %.chs,$(libHSgnomevfs_a_ALLSOURCES:.chs.pp=.chs))
libHSgnomevfs_a_CHSFILES_HS	= $(libHSgnomevfs_a_CHSFILES:.chs=.hs)
libHSgnomevfs_a_HSCFILES	= $(filter %.hsc, $(libHSgnomevfs_a_ALLSOURCES))
libHSgnomevfs_a_HSCFILES_HS	= $(libHSgnomevfs_a_HSCFILES:.hsc=.hs)
libHSgnomevfs_a_BUILDSOURCES	=			\
	$(libHSgnomevfs_a_HSPPFILES:.hs.pp=.hs)		\
	$(libHSgnomevfs_a_CHSPPFILES:.chs.pp=.chs)	\
	$(libHSgnomevfs_a_CHSFILES_HS)			\
	$(libHSgnomevfs_a_HSCFILES_HS)			\
	$(libHSgnomevfs_a_GENERATEDSOURCES)
libHSgnomevfs_a_HSFILES  =				\
	$(filter %.hs,$(libHSgnomevfs_a_BUILDSOURCES))	\
	$(filter %.hs,$(libHSgnomevfs_a_ALLSOURCES))
libHSgnomevfs_a_CFILES = $(filter %.c,$(libHSgnomevfs_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgnomevfs_a_HSFILES:.hs=.hi)

gnomevfs_MOSTLYCLEANFILES  = $(am_libHSgnomevfs_a_OBJECTS)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_HSFILES:.hs=.hi)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_CHSFILES:.chs=.chi)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_CHSFILES:.chs=.h)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_CHSFILES:.chs=_stub.h)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_CHSFILES:.chs=_stub.o)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_CHSFILES:.chs=_stub.c)
gnomevfs_CLEANFILES	   = $(libHSgnomevfs_a_BUILDSOURCES)
gnomevfs_CLEANFILES 	  += $(libHSgnomevfs_a_CHSFILES_HS:.hs=.dep)
gnomevfs_CLEANFILES += \
	$(libHSgnomevfs_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSgnomevfs_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSgnomevfs_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgnomevfs_a_CHSFILES_HS): $(libHSgnomevfs_a_PRECOMP)
$(libHSgnomevfs_a_CHSFILES:.chs=.dep) :		\
	$(libHSgnomevfs_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgnomevfs_a_CHSFILES:.chs=.dep) gnomevfs/libHSgnomevfs_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgnomevfs_a_AR = $(srcdir)/mk/link-splitobjs.sh
gnomevfs_CLEANFILES_SPLIT = $(libHSgnomevfs_a_HSFILES:.hs=_split/)
else
libHSgnomevfs_a_AR = $(AR) $(ARFLAGS)
endif

libHSgnomevfs_a_DEPENDENCIES = HSgnomevfs.o
pkglib_DATA += HSgnomevfs.o
HSgnomevfs.o : $(libHSgnomevfs_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgnomevfs_a_OBJECTS) $(libHSgnomevfs_a_LIBADD)

if ENABLE_PROFILING
libHSgnomevfs_a_DEPENDENCIES += libHSgnomevfs_p.a
pkglib_DATA += libHSgnomevfs_p.a
libHSgnomevfs_p.a : $(libHSgnomevfs_a_HSFILES:.hs=.p_o) \
	$(libHSgnomevfs_a_LIBADD:.o=.p_o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gnomevfs/libHSgnomevfs_a.p_deps
 endif

nobase_hi_DATA            += $(libHSgnomevfs_a_HSFILES:.hs=.p_hi)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_HSFILES:.hs=.p_hi)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_HSFILES:.hs=.p_o)
gnomevfs_MOSTLYCLEANFILES += $(libHSgnomevfs_a_LIBADD:.o=.p_o)

endif

endif

#
#      gstreamer package
#
################################################################################

if ENABLE_GSTREAMER

gstreamer_PKGNAME = libHSgstreamer_a

libHSgstreamer_a_NAME			= gstreamer
libHSgstreamer_a_CONFIG			= gstreamer/gstreamer.package.conf
libHSgstreamer_a_EXTERNALDEPS		= base-$(PKG_BASE_VERSION) mtl-$(PKG_MTL_VERSION)
if HAVE_SPLIT_BASE
libHSgstreamer_a_EXTERNALDEPS		+= bytestring-$(PKG_BYTESTRING_VERSION)
endif
libHSgstreamer_a_INTERNALDEPS		= glib
libHSgstreamer_a_HEADER			= gstreamer/hsgstreamer.h
libHSgstreamer_a_PRECOMP		= gstreamer/gstreamer.precomp
$(libHSgstreamer_a_PRECOMP): $(libHSgstreamer_a_HEADER)
libHSgstreamer_a_LIBS			= $(GLIB_LIBS) $(GSTREAMER_LIBS)
libHSgstreamer_a_HCFLAGS		= $(FFI)
libHSgstreamer_a_CFLAGS			= $(filter-out -I% -D%,$(GLIB_CFLAGS) $(GSTREAMER_CFLAGS))
libHSgstreamer_a_CPPFLAGS		= $(filter     -I% -D%,$(GLIB_CFLAGS) $(GSTREAMER_CFLAGS)) -I.

libHSgstreamer_a_SOURCESDIRS		= gstreamer

libHSgstreamer_a_LIBADD			=				\
	gstreamer/Media/Streaming/GStreamer/Core/Bus_stub.o		\
	gstreamer/Media/Streaming/GStreamer/Core/Index_stub.o		\
	gstreamer/Media/Streaming/GStreamer/Core/Iterator_stub.o	\
	gstreamer/Media/Streaming/GStreamer/Core/Registry_stub.o

if !USE_GCLOSUE_SIGNALS_IMPL
libHSgstreamer_a_LIBADD += gstreamer/Media/Streaming/GStreamer/Core/Signals_stub.o
endif

gstreamer/libHSgstreamer_a.deps	: $(libHSgstreamer_a_HSFILES) | glib/libHSglib_a.deps

libHSgstreamer_a_GENERATEDSOURCES	=				\
	gstreamer/Media/Streaming/GStreamer/Core/Signals.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Hierarchy.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/MiniHierarchy.chs	\
	gstreamer/Media/Streaming/GStreamer/Core/GObjectHierarchy.chs	\
	gstreamer/Media/Streaming/GStreamer/Base/Hierarchy.chs		\
	gstreamer/Media/Streaming/GStreamer/Base/GObjectHierarchy.chs	\
	gstreamer/Media/Streaming/GStreamer/Controller/GObjectHierarchy.chs	\
	gstreamer/Media/Streaming/GStreamer/Net/Hierarchy.chs		\
	gstreamer/Media/Streaming/GStreamer/Audio/Hierarchy.chs

nodist_libHSgstreamer_a_SOURCES		= $(libHSgstreamer_a_GENERATEDSOURCES)

libHSgstreamer_a_SOURCES		=               		\
	gstreamer/Media/Streaming/GStreamer/Core/HierarchyBase.hs	\
	gstreamer/Media/Streaming/GStreamer/Core/MiniHierarchyBase.chs	\
	gstreamer/Media/Streaming/GStreamer/Core/Types.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Constants.hsc		\
	gstreamer/Media/Streaming/GStreamer/Core/Init.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Object.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/MiniObject.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Element.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Pad.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/GhostPad.chs.pp	\
	gstreamer/Media/Streaming/GStreamer/Core/Bin.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Format.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/ElementFactory.chs.pp	\
	gstreamer/Media/Streaming/GStreamer/Core/Caps.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Structure.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Message.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Buffer.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Event.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Query.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Bus.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/Clock.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core/PadTemplate.chs.pp	\
	gstreamer/Media/Streaming/GStreamer/Core/ImplementsInterface.chs \
	gstreamer/Media/Streaming/GStreamer/Core/Index.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/IndexFactory.chs	\
	gstreamer/Media/Streaming/GStreamer/Core/Iterator.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Parse.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Pipeline.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Plugin.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/PluginFeature.chs	\
	gstreamer/Media/Streaming/GStreamer/Core/Registry.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/Segment.chs		\
	gstreamer/Media/Streaming/GStreamer/Core/SystemClock.chs	\
	gstreamer/Media/Streaming/GStreamer/Core/TagList.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Core.hs			\
	gstreamer/Media/Streaming/GStreamer/Base/Constants.hsc		\
	gstreamer/Media/Streaming/GStreamer/Base/Types.chs		\
	gstreamer/Media/Streaming/GStreamer/Base/BaseSrc.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Base/BaseSink.chs.pp	\
	gstreamer/Media/Streaming/GStreamer/Base/BaseTransform.chs.pp	\
	gstreamer/Media/Streaming/GStreamer/Base/PushSrc.chs		\
	gstreamer/Media/Streaming/GStreamer/Base/Adapter.chs.pp		\
	gstreamer/Media/Streaming/GStreamer/Base/CollectPads.chs	\
	gstreamer/Media/Streaming/GStreamer/Base.hs         		\
	gstreamer/Media/Streaming/GStreamer/Controller/Types.chs	\
	gstreamer/Media/Streaming/GStreamer/Controller/Controller.chs	\
	gstreamer/Media/Streaming/GStreamer/Net/Types.chs		\
	gstreamer/Media/Streaming/GStreamer/Net/NetClientClock.chs	\
	gstreamer/Media/Streaming/GStreamer/DataProtocol/Constants.hsc	\
	gstreamer/Media/Streaming/GStreamer/DataProtocol/DataProtocol.chs.pp \
	gstreamer/Media/Streaming/GStreamer/Audio/Constants.hsc		\
	gstreamer/Media/Streaming/GStreamer/Audio/Types.chs		\
	gstreamer/Media/Streaming/GStreamer/Audio/Audio.chs		\
	gstreamer/Media/Streaming/GStreamer/Audio/AudioClock.chs	\
	gstreamer/Media/Streaming/GStreamer.hs 	        		\
	gstreamer/hsgstreamer.c

gstreamer_Media_Streaming_GStreamer_Core_Types_hs_HCFLAGS	= -fglasgow-exts
gstreamer_Media_Streaming_GStreamer_Core_Caps_hs_HCFLAGS	= -fglasgow-exts

htmldoc_HSFILES_HIDDEN				+=			\
	$(libHSgstreamer_a_GENERATEDSOURCES:.chs=.hs)			\
	gstreamer/Media/Streaming/GStreamer/Core/MiniHierarchyBase.hs	\
	gstreamer/Media/Streaming/GStreamer/Core/Constants.hs		\
	gstreamer/Media/Streaming/GStreamer/Core/Types.hs		\
	gstreamer/Media/Streaming/GStreamer/Base/Types.hs		\
	gstreamer/Media/Streaming/GStreamer/Audio/Types.hs		\
	gstreamer/Media/Streaming/GStreamer/Controller/Types.hs		\
	gstreamer/Media/Streaming/GStreamer/Net/Types.hs

libHSgstreamer_a_ALLSOURCES			 = $(libHSgstreamer_a_SOURCES) $(nodist_libHSgstreamer_a_SOURCES)

gstreamer/Media/Streaming/GStreamer/Core/Hierarchy.chs :				\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-core --lib=gstreamer --prefix=gst				\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Core.Hierarchy)

gstreamer/Media/Streaming/GStreamer/Core/MiniHierarchy.chs :				\
	$(srcdir)/gstreamer/mini-hierarchy.list						\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/MiniHierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/mini-hierarchy.list						\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/MiniHierarchy.chs.template	\
	$@ --tag=gstreamer-core --lib=gstreamer --prefix=gst				\
	--root=MiniObject								\
	--modname=Media.Streaming.GStreamer.Core.MiniHierarchy)

gstreamer/Media/Streaming/GStreamer/Core/GObjectHierarchy.chs :				\
	$(srcdir)/gstreamer/gobject-hierarchy.list					\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/gobject-hierarchy.list					\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-core --lib=gstreamer --prefix=gst				\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Core.GObjectHierarchy)

gstreamer/Media/Streaming/GStreamer/Base/Hierarchy.chs :				\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-base --lib=gstreamer --prefix=gst				\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Base.Hierarchy				\
	--parentname=Media.Streaming.GStreamer.Core.Hierarchy)

gstreamer/Media/Streaming/GStreamer/Base/GObjectHierarchy.chs :				\
	$(srcdir)/gstreamer/gobject-hierarchy.list					\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/gobject-hierarchy.list					\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-base --lib=gstreamer --prefix=gst				\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Base.GObjectHierarchy)

gstreamer/Media/Streaming/GStreamer/Controller/GObjectHierarchy.chs :				\
	$(srcdir)/gstreamer/gobject-hierarchy.list					\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/gobject-hierarchy.list					\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-controller --lib=gstreamer --prefix=gst			\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Controller.GObjectHierarchy)

gstreamer/Media/Streaming/GStreamer/Net/Hierarchy.chs :				\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-net --lib=gstreamer --prefix=gst				\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Net.Hierarchy				\
	--parentname=Media.Streaming.GStreamer.Core.Hierarchy)

gstreamer/Media/Streaming/GStreamer/Core/Signals.chs : 			\
	$(srcdir)/tools/callbackGen/Signal.chs.template 		\
	$(srcdir)/gstreamer/marshal.list 				\
	$(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT)
	$(strip $(srcdir)/tools/callbackGen/HookGenerator$(EXEEXT) 	\
	$(srcdir)/gstreamer/marshal.list 				\
	$(srcdir)/tools/callbackGen/Signal.chs.template $@ 		\
	Media.Streaming.GStreamer.Core.Signals)

gstreamer/Media/Streaming/GStreamer/Audio/Hierarchy.chs :				\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)				\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template
	$(strip $(srcdir)/tools/hierarchyGenGst/TypeGenerator$(EXEEXT)			\
	$(srcdir)/gstreamer/hierarchy.list						\
	$(srcdir)/gstreamer/Media/Streaming/GStreamer/Hierarchy.chs.template		\
	$@ --tag=gstreamer-audio --lib=gstreamer --prefix=gst				\
	--root=GObject									\
	--modname=Media.Streaming.GStreamer.Audio.Hierarchy				\
	--parentname=Media.Streaming.GStreamer.Core.Hierarchy)

am_libHSgstreamer_a_OBJECTS			 =	\
	$(addsuffix .$(OBJEXT),$(basename $(basename $(libHSgstreamer_a_ALLSOURCES))))

libHSgstreamer_a_HSPPFILES	= $(filter %.hs.pp, $(libHSgstreamer_a_ALLSOURCES))
libHSgstreamer_a_CHSPPFILES	= $(filter %.chs.pp,$(libHSgstreamer_a_ALLSOURCES))
libHSgstreamer_a_CHSFILES	=			\
	$(filter %.chs,$(libHSgstreamer_a_ALLSOURCES:.chs.pp=.chs))
libHSgstreamer_a_CHSFILES_HS	= $(libHSgstreamer_a_CHSFILES:.chs=.hs)
libHSgstreamer_a_HSCFILES	= $(filter %.hsc, $(libHSgstreamer_a_ALLSOURCES))
libHSgstreamer_a_HSCFILES_HS	= $(libHSgstreamer_a_HSCFILES:.hsc=.hs)
libHSgstreamer_a_BUILDSOURCES	=			\
	$(libHSgstreamer_a_HSPPFILES:.hs.pp=.hs)	\
	$(libHSgstreamer_a_CHSPPFILES:.chs.pp=.chs)	\
	$(libHSgstreamer_a_CHSFILES_HS)			\
	$(libHSgstreamer_a_HSCFILES_HS)			\
	$(libHSgstreamer_a_GENERATEDSOURCES)
libHSgstreamer_a_HSFILES  =				\
	$(filter %.hs,$(libHSgstreamer_a_BUILDSOURCES))	\
	$(filter %.hs,$(libHSgstreamer_a_ALLSOURCES))
libHSgstreamer_a_CFILES = $(filter %.c,$(libHSgstreamer_a_ALLSOURCES))

nobase_hi_DATA += $(libHSgstreamer_a_HSFILES:.hs=.hi)

gstreamer_MOSTLYCLEANFILES  = $(am_libHSgstreamer_a_OBJECTS)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_HSFILES:.hs=.hi)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_CHSFILES:.chs=.chi)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_CHSFILES:.chs=.h)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_CHSFILES:.chs=_stub.h)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_CHSFILES:.chs=_stub.o)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_CHSFILES:.chs=_stub.c)
gstreamer_CLEANFILES	   = $(libHSgstreamer_a_BUILDSOURCES)
gstreamer_CLEANFILES 	  += $(libHSgstreamer_a_CHSFILES_HS:.hs=.dep)
gstreamer_CLEANFILES += \
	$(libHSgstreamer_a_HSCFILES:.hsc=_hsc_make$(EXEEXT))		\
	$(libHSgstreamer_a_HSCFILES:.hsc=_hsc_make.c)			\
	$(libHSgstreamer_a_HSCFILES:.hsc=_hsc_make.o)

$(libHSgstreamer_a_CHSFILES_HS): $(libHSgstreamer_a_PRECOMP)
$(libHSgstreamer_a_CHSFILES:.chs=.dep) :	\
	$(libHSgstreamer_a_GENERATEDSOURCES)

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include $(libHSgstreamer_a_CHSFILES:.chs=.dep) gstreamer/libHSgstreamer_a.deps
 endif

if ENABLE_SPLITOBJS
libHSgstreamer_a_AR = $(srcdir)/mk/link-splitobjs.sh
gstreamer_CLEANFILES_SPLIT = $(libHSgstreamer_a_HSFILES:.hs=_split/)
else
libHSgstreamer_a_AR = $(AR) $(ARFLAGS)
endif

libHSgstreamer_a_DEPENDENCIES = HSgstreamer.o
pkglib_DATA += HSgstreamer.o
HSgstreamer.o : $(libHSgstreamer_a_OBJECTS)
	$(LD) -r $(LD_X) -o $@ $(libHSgstreamer_a_OBJECTS) $(libHSgstreamer_a_LIBADD)

if ENABLE_PROFILING
libHSgstreamer_a_DEPENDENCIES += libHSgstreamer_p.a
pkglib_DATA += libHSgstreamer_p.a
libHSgstreamer_p.a :				\
	$(libHSgstreamer_a_HSFILES:.hs=.p_o)	\
	$(libHSgstreamer_a_LIBADD:.o=.p_o)	\
	$(libHSgstreamer_a_CFILES:.c=.o)
	$(AR) $(ARFLAGS) $@ $^

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
-include gstreamer/libHSgstreamer_a.p_deps
 endif

nobase_hi_DATA            += $(libHSgstreamer_a_HSFILES:.hs=.p_hi)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_HSFILES:.hs=.p_hi)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_HSFILES:.hs=.p_o)
gstreamer_MOSTLYCLEANFILES += $(libHSgstreamer_a_LIBADD:.o=.p_o)

endif

endif

#
#       inplace package registration
#
################################################################################

# As of GHC 6.6 it is no longer possible to build multiple packages without
# registering dependent ones first. So what we have to do now is to create a
# package.conf.inplace package db and register all our packages in it. Then we
# use this package db when building the other packages.

package.conf.inplace:
	echo "[]" > $@
	$(foreach pkg,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$${pkglibdir}/imports|.|' \
		-e 's|"$${pkglibdir}/include"|.|' \
		-e 's|$${pkglibdir}|.|' \
		-e 's|$${modules}|$(call getModules,$(pkg))|' \
		$(call getVar,$(pkg),CONFIG) \
		| $(GHCPKG) update - --force --package-conf=$@; )

.DELETE_ON_ERROR: package.conf.inplace

CLEANFILES += package.conf.inplace package.conf.inplace.old

#
#	documentation
#
################################################################################

htmldoc_images =								  \
	docs/reference/images/stock-icons/stock_about_24.png			  \
	docs/reference/images/stock-icons/stock_add_24.png			  \
	docs/reference/images/stock-icons/stock_align_center_24.png		  \
	docs/reference/images/stock-icons/stock_align_justify_24.png		  \
	docs/reference/images/stock-icons/stock_align_left_24.png		  \
	docs/reference/images/stock-icons/stock_align_right_24.png		  \
	docs/reference/images/stock-icons/stock_apply_20.png			  \
	docs/reference/images/stock-icons/stock_bottom_24.png			  \
	docs/reference/images/stock-icons/stock_broken_image_24.png		  \
	docs/reference/images/stock-icons/stock_cancel_20.png			  \
	docs/reference/images/stock-icons/stock_cdrom_24.png			  \
	docs/reference/images/stock-icons/stock_clear_24.png			  \
	docs/reference/images/stock-icons/stock_close_24.png			  \
	docs/reference/images/stock-icons/stock_color_picker_25.png		  \
	docs/reference/images/stock-icons/stock_colorselector_24.png		  \
	docs/reference/images/stock-icons/stock_connect_24.png			  \
	docs/reference/images/stock-icons/stock_convert_24.png			  \
	docs/reference/images/stock-icons/stock_copy_24.png			  \
	docs/reference/images/stock-icons/stock_cut_24.png			  \
	docs/reference/images/stock-icons/stock_dialog_authentication_48.png	  \
	docs/reference/images/stock-icons/stock_dialog_error_48.png		  \
	docs/reference/images/stock-icons/stock_dialog_info_48.png		  \
	docs/reference/images/stock-icons/stock_dialog_question_48.png		  \
	docs/reference/images/stock-icons/stock_dialog_warning_48.png		  \
	docs/reference/images/stock-icons/stock_directory_24.png		  \
	docs/reference/images/stock-icons/stock_disconnect_24.png		  \
	docs/reference/images/stock-icons/stock_dnd_32.png			  \
	docs/reference/images/stock-icons/stock_dnd_multiple_32.png		  \
	docs/reference/images/stock-icons/stock_down_arrow_24.png		  \
	docs/reference/images/stock-icons/stock_edit_24.png			  \
	docs/reference/images/stock-icons/stock_exec_24.png			  \
	docs/reference/images/stock-icons/stock_exit_24.png			  \
	docs/reference/images/stock-icons/stock_file_24.png			  \
	docs/reference/images/stock-icons/stock_first_24.png			  \
	docs/reference/images/stock-icons/stock_font_24.png			  \
	docs/reference/images/stock-icons/stock_fullscreen_24.png		  \
	docs/reference/images/stock-icons/stock_harddisk_24.png			  \
	docs/reference/images/stock-icons/stock_help_24.png			  \
	docs/reference/images/stock-icons/stock_home_24.png			  \
	docs/reference/images/stock-icons/stock_index_24.png			  \
	docs/reference/images/stock-icons/stock_info_24.png			  \
	docs/reference/images/stock-icons/stock_jump_to_24.png			  \
	docs/reference/images/stock-icons/stock_last_24.png			  \
	docs/reference/images/stock-icons/stock_leave_fullscreen_24.png		  \
	docs/reference/images/stock-icons/stock_left_arrow_24.png		  \
	docs/reference/images/stock-icons/stock_media_forward_24.png		  \
	docs/reference/images/stock-icons/stock_media_next_24.png		  \
	docs/reference/images/stock-icons/stock_media_pause_24.png		  \
	docs/reference/images/stock-icons/stock_media_play_24.png		  \
	docs/reference/images/stock-icons/stock_media_previous_24.png		  \
	docs/reference/images/stock-icons/stock_media_record_24.png		  \
	docs/reference/images/stock-icons/stock_media_rewind_24.png		  \
	docs/reference/images/stock-icons/stock_media_stop_24.png		  \
	docs/reference/images/stock-icons/stock_network_24.png			  \
	docs/reference/images/stock-icons/stock_new_24.png			  \
	docs/reference/images/stock-icons/stock_no_20.png			  \
	docs/reference/images/stock-icons/stock_ok_20.png			  \
	docs/reference/images/stock-icons/stock_open_24.png			  \
	docs/reference/images/stock-icons/stock_paste_24.png			  \
	docs/reference/images/stock-icons/stock_preferences_24.png		  \
	docs/reference/images/stock-icons/stock_print_24.png			  \
	docs/reference/images/stock-icons/stock_print_preview_24.png		  \
	docs/reference/images/stock-icons/stock_properties_24.png		  \
	docs/reference/images/stock-icons/stock_redo_24.png			  \
	docs/reference/images/stock-icons/stock_refresh_24.png			  \
	docs/reference/images/stock-icons/stock_remove_24.png			  \
	docs/reference/images/stock-icons/stock_revert_24.png			  \
	docs/reference/images/stock-icons/stock_right_arrow_24.png		  \
	docs/reference/images/stock-icons/stock_save_24.png			  \
	docs/reference/images/stock-icons/stock_save_as_24.png			  \
	docs/reference/images/stock-icons/stock_search_24.png			  \
	docs/reference/images/stock-icons/stock_search_replace_24.png		  \
	docs/reference/images/stock-icons/stock_sort_ascending_24.png		  \
	docs/reference/images/stock-icons/stock_sort_descending_24.png		  \
	docs/reference/images/stock-icons/stock_spellcheck_24.png		  \
	docs/reference/images/stock-icons/stock_stop_24.png			  \
	docs/reference/images/stock-icons/stock_text_bold_24.png		  \
	docs/reference/images/stock-icons/stock_text_indent_24.png		  \
	docs/reference/images/stock-icons/stock_text_italic_24.png		  \
	docs/reference/images/stock-icons/stock_text_strikethrough_24.png	  \
	docs/reference/images/stock-icons/stock_text_underline_24.png		  \
	docs/reference/images/stock-icons/stock_text_unindent_24.png		  \
	docs/reference/images/stock-icons/stock_top_24.png			  \
	docs/reference/images/stock-icons/stock_trash_24.png			  \
	docs/reference/images/stock-icons/stock_undelete_24.png			  \
	docs/reference/images/stock-icons/stock_undo_24.png			  \
	docs/reference/images/stock-icons/stock_up_arrow_24.png			  \
	docs/reference/images/stock-icons/stock_yes_20.png			  \
	docs/reference/images/stock-icons/stock_zoom_1_24.png			  \
	docs/reference/images/stock-icons/stock_zoom_fit_24.png			  \
	docs/reference/images/stock-icons/stock_zoom_in_24.png			  \
	docs/reference/images/stock-icons/stock_zoom_out_24.png
EXTRA_DIST += $(htmldoc_images)

if BUILDDOCS

# For now just build all the docs together, we can think about doing it in a
# more modular fashion later if it turns out to be useful.

htmldocdir = $(datadir)/doc/gtk2hs/html

PACKAGES = $(foreach PACKAGE,$(pkglib_LIBRARIES), $(patsubst libHS%.a,%,$(PACKAGE)))

htmldoc_haddock_files =								  \
	$(foreach HSFILE, $(filter-out						  \
	$(htmldoc_HSFILES_HIDDEN),$(htmldoc_HSFILES)),				  \
	    $(patsubst $(firstword $(subst /,					  \
	    ,$(HSFILE)))-%.hs,							  \
	     docs/reference/%.html,						  \
		$(subst /,-,$(HSFILE:.uncpp=))))				  \
	docs/reference/haddock.css docs/reference/haskell_icon.gif		  \
	docs/reference/plus.gif docs/reference/minus.gif			  \
	docs/reference/haddock.js						  \
	docs/reference/index.html docs/reference/doc-index.html			  \
	docs/reference/gtk2hs.haddock

if HADDOCK2
htmldoc_haddock_files +=							  \
	$(foreach HSFILE, $(filter-out						  \
	$(htmldoc_HSFILES_HIDDEN),$(htmldoc_HSFILES)),				  \
	    $(patsubst $(firstword $(subst /,					  \
	    ,$(HSFILE)))-%.hs,							  \
	     docs/reference/mini_%.html,					  \
		$(subst /,-,$(HSFILE:.uncpp=))))				  \
	docs/reference/haddock-util.js						  \
	docs/reference/frames.html						  \
	docs/reference/index-frames.html
endif

htmldoc_haddock_files +=							  \
	$(foreach LETTER, A B C D E F G H I J K L M N O P Q R S T U V W X Y	  \
		58 , docs/reference/doc-index-$(LETTER).html) #missing Z < > |, i.e. 60 62 124


htmldoc_DATA = $(htmldoc_haddock_files) $(htmldoc_images)

if !HADDOCK2

htmldoc_HSFILES =  $(filter-out $(htmldoc_HSFILES_EXCLUDE),	\
		$(foreach PACKAGE,$(PACKAGES),$(libHS$(PACKAGE)_a_HSFILES)))

$(htmldoc_haddock_files) : docs/reference/stamp
docs/reference/stamp : $(htmldoc_HSFILES)
	mkdir -p docs/reference
	$(strip $(HADDOCK) --html --odir=docs/reference \
		--title="Gtk2Hs" \
		--dump-interface=docs/reference/gtk2hs.haddock \
		--prologue=docs/prologue.txt \
		$(addprefix --use-package=,$(HADDOCK_PACKAGES)) \
		$(HADDOCK_FLAGS) $^)
	touch docs/reference/stamp
CLEANFILES+=docs/reference/stamp
else

htmldoc_HSFILES =  $(foreach PACKAGE,$(PACKAGES),$(libHS$(PACKAGE)_a_HSFILES))

htmldoc_HSFILES_AM = $(foreach HSFILE, $(htmldoc_HSFILES), $(subst .,_,$(subst /,_,$(HSFILE))))

all_externaldeps = $(sort $(foreach DEPPKG,$(PACKAGES),$(libHS$(DEPPKG)_a_EXTERNALDEPS)))

if !WIN32
htmldoc_haddock_interfaces = \
	$(foreach DEPPKG,$(all_externaldeps),$(shell $(GHCPKG) field $(DEPPKG) haddock-interfaces | sed 's,^[^:]*:,,' ))
else
htmldoc_haddock_interfaces = \
	$(foreach DEPPKG,$(all_externaldeps),$(shell $(GHCPKG) field $(DEPPKG) haddock-interfaces | sed -e 's,^[^:]*:,,' -e 's,\\,/,g' ))
endif

$(htmldoc_haddock_files) : docs/reference/stamp
docs/reference/stamp : $(htmldoc_HSFILES)
	mkdir -p docs/reference
	$(strip $(HADDOCK) --html --odir=docs/reference \
		--title="Gtk2Hs" \
		--dump-interface=docs/reference/gtk2hs.haddock \
		--prologue=docs/prologue.txt \
	        $(addprefix --read-interface=, $(htmldoc_haddock_interfaces)) \
		$(addprefix --optghc=,$(sort $(foreach PACKAGE, $(PACKAGES), $(libHS$(PACKAGE)_a_HCFLAGS) $(libHS$(PACKAGE)_a_CPPFLAGS))) \
		                      $(sort $(foreach HSFILE, $(htmldoc_HSFILES_AM), $($(HSFILE)_HCFLAGS)))) \
		$(HADDOCK_FLAGS) $^)
	touch docs/reference/stamp
CLEANFILES+=docs/reference/stamp
endif

doc_CLEANFILES  = $(htmldoc_haddock_files)

docdistdir = $(PACKAGE)-docs-$(VERSION)
dist-docs : $(htmldoc_DATA)
	ln -s docs/reference $(docdistdir)
	$(AMTAR) -c $(docdistdir) -hzf $(docdistdir).tar.gz
	rm $(docdistdir)

endif #BUILDDOCS

#
#	demos
#
################################################################################

DEMOS =

if ENABLE_GTK
DEMOS += demo/hello demo/buttonbox demo/concurrent \
	demo/graphic demo/unicode demo/soe
if HAVE_GTK_VERSION_2_2
DEMOS += demo/fastdraw
endif
if ENABLE_LIBGLADE
DEMOS += demo/glade demo/calc demo/noughty
if HAVE_GTK_VERSION_2_4
DEMOS += demo/filechooser demo/treeList
endif
if HAVE_GTK_VERSION_2_6
DEMOS += demo/profileviewer
endif
endif
if HAVE_GTK_VERSION_2_6
DEMOS += demo/actionMenu demo/carsim
endif
if HAVE_GTK_VERSION_2_8
if ENABLE_CAIRO
DEMOS += demo/cairo
endif
endif
if HAVE_GTK_VERSION_2_10
DEMOS += demo/statusicon
endif
endif
if ENABLE_GCONF
DEMOS += demo/gconf
endif
if ENABLE_SOURCEVIEW
DEMOS += demo/sourceview
endif
if ENABLE_MOZEMBED
DEMOS += demo/mozembed
endif
if ENABLE_GTKGLEXT
DEMOS += demo/opengl
endif
if ENABLE_GSTREAMER
DEMOS += demo/gstreamer
endif
if ENABLE_GNOMEVFS
DEMOS += demo/gnomevfs
endif

# For the install check just make sure all the demo programs compile and link
DEMO_MAKE_ARGS= $(if $(PKGCONF), HCFLAGS="-package-conf $(PKGCONF)") HC=$(HC)

installcheck-local:
	$(foreach DEMO,$(DEMOS),$(strip	\
		$(MAKE) -C $(DEMO) clean &&\
		$(MAKE) -C $(DEMO) $(DEMO_MAKE_ARGS) &&	\
	)) echo All demos compiled ok, now try running one or two...

EXTRA_DIST +=								\
	demo/buttonbox/ButtonBox.hs demo/buttonbox/Makefile		\
	demo/concurrent/Progress.hs demo/concurrent/Makefile		\
		demo/concurrent/ProgressThreadedRTS.hs			\
	demo/filechooser/FileChooserDemo.hs demo/filechooser/Makefile	\
		demo/filechooser/FileChooserDemo.glade			\
	demo/gconf/GConfDemo.hs demo/gconf/Makefile			\
	demo/glade/GladeTest.hs demo/glade/Makefile			\
		demo/glade/simple.glade 				\
	demo/graphic/Drawing.hs demo/graphic/Makefile			\
	demo/hello/World.hs demo/hello/Makefile 			\
	demo/mozembed/TestEmbedMoz.hs demo/mozembed/Makefile		\
	demo/profileviewer/ProfileViewer.hs				\
		demo/profileviewer/ParseProfile.hs			\
		demo/profileviewer/ProfileViewer.glade			\
		demo/profileviewer/Makefile				\
	demo/sourceview/SourceViewTest.hs demo/sourceview/Makefile	\
		demo/sourceview/haskell.lang				\
	demo/treeList/ListDemo.hs demo/treeList/TreeDemo.hs		\
		demo/treeList/ListTest.hs demo/treeList/ListTest.glade	\
		demo/treeList/TreeTest.hs demo/treeList/TreeTest.glade	\
		demo/treeList/DirList.hs demo/treeList/Completion.hs	\
		demo/treeList/ListDND.hs				\
		demo/treeList/resListDND/desktop.png			\
		demo/treeList/resListDND/laptop.png			\
		demo/treeList/resListDND/laptopSmall.png		\
		demo/treeList/resListDND/printer.png			\
		demo/treeList/resListDND/server.png			\
		demo/treeList/resListDND/tower.png			\
		demo/treeList/Makefile					\
	demo/unicode/Arabic.hs demo/unicode/Makefile			\
	demo/calc/CalcModel.hs demo/calc/Calc.hs demo/calc/Makefile	\
		demo/calc/calc.glade					\
	demo/fastdraw/FastDraw.hs demo/fastdraw/Makefile		\
	demo/actionMenu/ActionMenu.hs demo/actionMenu/Makefile		\
	demo/cairo/CairoGhci.hs demo/cairo/Clock.hs			\
		demo/cairo/Drawing.hs demo/cairo/Drawing2.hs		\
		demo/cairo/Graph.hs demo/cairo/StarAndRing.hs		\
		demo/cairo/Text.hs demo/cairo/Makefile 			\
		demo/cairo/cairo-clock-icon.png				\
	demo/opengl/RotatingCube.hs demo/opengl/Makefile		\
	demo/carsim/CarSim.hs demo/carsim/Makefile			\
	demo/fonts/Fonts.hs demo/fonts/Makefile				\
	demo/pango/Layout.hs demo/pango/Makefile			\
	demo/noughty/Noughty.hs demo/noughty/NoughtyGlade.hs		\
		demo/noughty/noughty.glade demo/noughty/License		\
		demo/noughty/Nought.png demo/noughty/Cross.png		\
		demo/noughty/Makefile					\
	demo/svg/Makefile  demo/svg/Svg2Png.hs  demo/svg/SvgViewer.hs	\
	demo/soe/BouncingBall.hs demo/soe/Demo1.hs demo/soe/Demo2.hs	\
		demo/soe/Snowflake.hs demo/soe/Makefile			\
	demo/statusicon/StatusIcon.hs demo/statusicon/Makefile		\
	demo/gstreamer/VorbisPlay.hs demo/gstreamer/Makefile		\
	demo/gnomevfs/TestDir.hs					\
		demo/gnomevfs/TestDriveVolume.hs			\
		demo/gnomevfs/TestSync.hs				\
		demo/gnomevfs/TestVolumeMonitor.hs			\
		demo/gnomevfs/TestXfer.hs				\
		demo/gnomevfs/Makefile



#
#       installing and registering packages with the Haskell compiler
#
################################################################################

# Include all the .pkg/.package.conf files in the install set, they're useful
# to distributors and if your installation gets corrupt you can re-register the
# packages from them.
pkglib_DATA += $(foreach pkgname,$(pkglib_LIBRARIES), $(call getVar,$(pkgname),CONFIG))

# Before we install the .pkg/.package.conf files we need to substitute for the
# ${pkglibdir} and ${modules} variables.
if !WIN32_INSTALLER
install-exec-local:
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$${pkglibdir}|$(pkglibdir)|g' \
		-e 's|$${modules}|$(call getModules,$(pkgname))|' \
		$(call getVar,$(pkgname),CONFIG) \
	> $(call getVar,$(pkgname),CONFIG).tmp; \
	mv $(call getVar,$(pkgname),CONFIG).tmp $(call getVar,$(pkgname),CONFIG); \
	$(SED)	's|$${modules}|$(call getModules,$(pkgname))|' \
		$(patsubst %.package.conf,%.cabal,$(call getVar,$(pkgname),CONFIG)) \
	> $(patsubst %.package.conf,%.cabal,$(call getVar,$(pkgname),CONFIG)).tmp; \
	mv $(patsubst %.package.conf,%.cabal,$(call getVar,$(pkgname),CONFIG)).tmp \
	   $(patsubst %.package.conf,%.cabal,$(call getVar,$(pkgname),CONFIG));)

else WIN32_INSTALLER
# Except on Windows if we're makeing an installer where we delay the variable
# substitution, since the installer does that. So we still substitute for the
# ${modules} variable, but we don't substitute for ${pkglibdir} and we
# "reverse" substitute occurences of the Gtk installation prefix for
# ${GTK_BASEPATH} (that is the string literal, not the value of the variable)
#
# Annother annoying thing is that the .lib names and .dll names on windows do
# not match up, so we have to specify different libraries for ghci than for
# ghc. Eg while it's gtk-2.0.lib it's libgtk-2.0-0.dll.
# We generate the "extra-ghci-libraries:" field from the "extra-libraries:"
# field using a bit of cunning sed hackery.
install-exec-local:
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(SED)	-e 's|$(GTK_BASEPATH)|$${GTK_BASEPATH}|g' \
		-e 's|$${modules}|$(call getModules,$(pkgname))|' \
		-e '/^extra-libraries:/ \
			p; \
			s/extra-libraries/extra-ghci-libraries/; \
			s/"\([a-z0-9._-]*\.0\)"/"lib\1-0"/g; \
			s/"cairo"/"libcairo-2"/; \
			s/"rsvg-2"/"librsvg-2-2"/; \
			s/"xml2"/"libxml2"/' \
		$(call getVar,$(pkgname),CONFIG) \
	> $(call getVar,$(pkgname),CONFIG).tmp; \
	mv $(call getVar,$(pkgname),CONFIG).tmp $(call getVar,$(pkgname),CONFIG);)
endif WIN32_INSTALLER

COMMA := ,
getModules =									\
	$(subst $(SPACE),$(COMMA) ,$(sort					\
		$(foreach HSFILE,$(call getVar,$(1),HSFILES), 			\
			$(patsubst $(firstword $(subst /, ,$(HSFILE))).%.hs, %,	\
				$(subst /,.,$(HSFILE))				\
			)							\
		)								\
	))

if ENABLE_PKGREG

# For distributors we usually do not want to register the packages using
# ghc-pkg at install time. That is usually left for a post-install action.
# Therefore these actions can be disabled by ./configure --without-pkgreg

install-data-hook :
	$(if $(PKGCONF),if test -f $(PKGCONF); then :; \
	else echo "[]" > $(PKGCONF); fi;)
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	$(GHCPKG) $(addprefix --package-conf=,$(PKGCONF)) \
	$(if $(USERPKGCONF),--user) \
	update $(DESTDIR)$(pkglibdir)/$(patsubst libHS%.a,%.package.conf,$(pkgname));)

uninstall-hook :
	$(foreach pkgname,$(pkglib_LIBRARIES), \
	  $(GHCPKG) $(addprefix --package-conf=,$(PKGCONF)) $(if $(USERPKGCONF),--user) \
	  unregister $(patsubst libHS%.a,%,$(pkgname))-$(PACKAGE_VERSION);) \
	$(if $(PKGCONF),if test -f $(PKGCONF); then \
	  if test -n `head $(PKGCONF) | $(GREP) -e "\[\]"`; then \
	  rm -f $(PKGCONF) $(PKGCONF).old; fi; \
	fi)

else !ENABLE_PKGREG

install-data-hook :
uninstall-hook :

endif !ENABLE_PKGREG

#
#	cleaning
#
################################################################################

# We have so many files to clean we can't do it all in on go as it overflows
# the command line length. So we have to do it in batches:

mostlyclean: mostlyclean-am
	rm -f $(c2hs_MOSTLYCLEANFILES)
	rm -f $(glib_MOSTLYCLEANFILES)
	rm -f $(gtk_MOSTLYCLEANFILES)
	rm -f $(gio_MOSTLYCLEANFILES)
	rm -f $(glade_MOSTLYCLEANFILES)
	rm -f $(gconf_MOSTLYCLEANFILES)
	rm -f $(sourceview_MOSTLYCLEANFILES)
	rm -f $(gtksourceview2_MOSTLYCLEANFILES)
	rm -f $(mozembed_MOSTLYCLEANFILES)
	rm -f $(cairo_MOSTLYCLEANFILES)
	rm -f $(svgcairo_MOSTLYCLEANFILES)
	rm -f $(gtkglext_MOSTLYCLEANFILES)
	rm -f $(soegtk_MOSTLYCLEANFILES)
	rm -f $(gnomevfs_MOSTLYCLEANFILES)
	rm -f $(gstreamer_MOSTLYCLEANFILES)
	rm -f $(doc_MOSTLYCLEANFILES)

clean: mostlyclean clean-am
	rm -f  $(c2hs_CLEANFILES)
	rm -rf $(c2hs_CLEANFILES_SPLIT)
	rm -f  $(glib_CLEANFILES)
	rm -rf $(glib_CLEANFILES_SPLIT)
	rm -f  $(gtk_CLEANFILES)
	rm -rf $(gtk_CLEANFILES_SPLIT)
	rm -f  $(gio_CLEANFILES)
	rm -rf $(gio_CLEANFILES_SPLIT)
	rm -f  $(glade_CLEANFILES)
	rm -rf $(glade_CLEANFILES_SPLIT)
	rm -f  $(gconf_CLEANFILES)
	rm -rf $(gconf_CLEANFILES_SPLIT)
	rm -f  $(sourceview_CLEANFILES)
	rm -rf $(sourceview_CLEANFILES_SPLIT)
	rm -f  $(gtksourceview2_CLEANFILES)
	rm -rf $(gtksourceview2_CLEANFILES_SPLIT)
	rm -f  $(mozembed_CLEANFILES)
	rm -rf $(mozembed_CLEANFILES_SPLIT)
	rm -f  $(cairo_CLEANFILES)
	rm -rf $(cairo_CLEANFILES_SPLIT)
	rm -f  $(svgcairo_CLEANFILES)
	rm -rf $(svgcairo_CLEANFILES_SPLIT)
	rm -f  $(gtkglext_CLEANFILES)
	rm -rf $(gtkglext_CLEANFILES_SPLIT)
	rm -f  $(soegtk_CLEANFILES)
	rm -rf $(soegtk_CLEANFILES_SPLIT)
	rm -f  $(gnomevfs_CLEANFILES)
	rm -rf $(gnomevfs_CLEANFILES_SPLIT)
	rm -f  $(gstreamer_CLEANFILES)
	rm -rf $(gstreamer_CLEANFILES_SPLIT)
	rm -f  $(doc_CLEANFILES)

distclean: clean distclean-am
	-rm -f $(am__CONFIG_DISTCLEAN_FILES)
	-rm -f Makefile

maintainer-clean: distclean maintainer-clean-am
	-rm -rf $(top_srcdir)/autom4te.cache

include mk/common.mk
