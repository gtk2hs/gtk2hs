TOP = ..

include $(TOP)/mk/config.mk

SUBDIRS = base/admin base/errors base/general base/graphs base/state \
	  base/syms base/syntax base/sysdep c chs gen state toplevel

APPNAME = c2hs

NEEDPACKAGES = lang

EXTRAHC_FLAGS = -fglasgow-exts

EXTRA_CFILES = toplevel/c2hs_config.c

EXTRA_HSFILES = toplevel/C2HSConfig.hs

EXTRA_TARFILES = toplevel/C2HSConfig.hs.in

include $(TOP)/mk/common.mk

