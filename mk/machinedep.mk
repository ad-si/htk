# This file, included by Make, contains variables you may need to
# change for different installations of Uni.
#
# HCHOME needs to be set to Glasgow Haskell's installation directory. 
HCHOME = /usr/local/pub-bkb/ghc/ghc.03.09.04
# SOCKETLIB is the Unix library for the networking primitives.
#
# At the moment Sparc-Solaris requires 
#   SOCKETLIB = -lsocket
# and Linux requires 
#   SOCKETLIB =
#
SOCKETLIB = -lsocket

