#
# Configuration Section
#

sysarch := $(shell uname -s -r)
hkvers  := 20x
noti    := 1

ifeq (no,$(NOTI))
noti := 0
endif
ifeq (yes,$(NOTI))
noti := 1
endif

ifeq (20x,$(HK_VERS))
hkvers := 20x
endif
ifeq (029,$(HK_VERS))
hkvers := 029
endif

ifeq (SunOS,$(findstring SunOS,$(sysarch)))
conf_solaris := 1
conf_linux   := 0
endif
ifeq (Linux,$(findstring Linux,$(sysarch)))
conf_solaris := 0
conf_linux   := 1
endif

ifeq (029,$(hkvers))
conf_029  := 1
conf_20x  := 0
endif
ifeq (20x,$(hkvers))
conf_029  := 0
conf_20x  := 1
endif


