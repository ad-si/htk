#################################################################################
#
# This one file should be included (directly or indirectly) by all Makefiles 
# 
#
#################################################################################

# This rule makes sure that "all" is the default target, regardless of where it appears
#		THIS RULE MUST REMAIN FIRST!
default: all

# 		Now follow the pieces of boilerplate
#		The "-" signs tell make not to complain if they don't exist


include top/mk/config.mk
# All configuration information
#


include top/mk/var.mk
# Variables that say where things belong (e.g install directories)
# and where we are right now
# Also defines variables for standard files (SRCS, LIBS etc)


include top/mk/suffix.mk
# suffix.mk includes the rules that actually build things.


