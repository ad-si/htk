dnl == autoconf source for the UniForM workbench ==
dnl some details stolen from GHC configure.in file.
# Check that sources are available
AC_INIT(mk/suffix.mk)
#
# Is this configure script being run in a binary release or not?
if test -r "not.a.binary.release"; then
   echo Configuring for building from source 
   BINARY=NO
else
   echo Configuring a binary release
   BINARY=YES
fi
AC_SUBST(BINARY)
#
#
# Determine how much we shall try to build.  At the moment this is
# simply a number BUILDAMOUNT which has the following meanings:
#
# 100 => everything
# 70 => Het
# 50 => only HTk
# It defaults to 100.  If it is set lower, we will build less.  We may build
# less anyway, if we can't find all the required programs, for example the
# Berkeley Data Base.
BUILDAMOUNT=100

AC_ARG_ENABLE(
   MMiSS,
   AC_HELP_STRING(
      [--enable-MMiSS],
      [Compile everything needed for MMiSS]
      ),
   BUILDAMOUNT=100,
   )

AC_ARG_ENABLE(
   Het,
   AC_HELP_STRING(
      [--enable-Het],
      [Compile everything needed for HetCATS]
      ),
   BUILDAMOUNT=70,
   )

AC_ARG_ENABLE(
   HTk,
   AC_HELP_STRING(
      [--enable-HTk],
      [Compile everything needed for HTk]
      ),
   BUILDAMOUNT=50,
   )

AC_SUBST(BUILDAMOUNT)
   
# Set GOAHEAD to NO prevent modifying Makefiles and so on.
GOAHEAD=YES
#
# Get  the top of the build tree
TOP=`pwd`
echo ""
echo "The top of the UniForM build tree is: $TOP"
AC_SUBST(TOP)
# 
#
# Are we running Windows or Unix?
case `uname` in
   CYGWIN* ) WINDOWS=1 ; OSTITLE=windows ;;
   MINGW32* ) WINDOWS=1 ; OSTITLE=windows ;;
   Linux* ) WINDOWS=0 ; OSTITLE=linux ;;
   SunOS* ) WINDOWS=0 ; OSTITLE=solaris ;;
   FreeBSD* ) WINDOWS=0 ; OSTITLE=freebsd ;;
   Darwin*) WINDOWS=0 ; OSTITLE=MacOS ;;
   * ) WINDOWS=0; OSTITLE=unknown ;;
esac

echo You are running $OSTITLE

AC_SUBST(WINDOWS)
AC_SUBST(OSTITLE)
#
# What is our version?
UNIVERSION=`cat VERSION`
AC_SUBST(UNIVERSION)
#
# Find a C compiler.
AC_PATH_PROGS(CC,gcc cc,)
#
# Find sed
AC_PATH_PROGS(SED,sed)
if test -z "$SED"; then
   echo "Without sed we won't be able to finish this configuration"
   GOAHEAD=NO
fi
#
# Find a GNU linker
AC_PATH_PROGS(LD,gnu-ld gld ld,)
$LD --version
echo
echo "If that message didn't indicate a GNU linker, it won't be possible to use ghci on packages constructed with this build."

#
# Check for gmake
AC_PATH_PROGS(GMAKE,gmake,make)
$GMAKE --version
#
echo "You need GNU make to compile uni".
#
# Check for ghc
AC_PATH_PROGS(GHC,ghc)
if test -z "$GHC"; then
   if test "$BINARY" = NO; then
      echo "Without a recent version of Glasgow Haskell you cannot compile UniForm"
      GOAHEAD=NO
   fi
   GhcShortVersion=000
else
   UNI_GHC_VERSION([GhcVersion],[GhcMajVersion],[GhcMinVersion],[GhcPatchLevel])
   AC_SUBST(GhcVersion)
   AC_SUBST(GhcMajVersion)
   AC_SUBST(GhcMinVersion)
   AC_SUBST(GhcPatchLevel)

   # Turn GhcMajVersion & GhcMinVersion into a single three-digit number we
   # can compare against.
   if test $GhcMinVersion -lt 10
   then
      GhcShortVersion=${GhcMajVersion}0$GhcMinVersion
   else
      GhcShortVersion=${GhcMajVersion}$GhcMinVersion
   fi

   echo GhcVersion=$GhcVersion GhcShortVersion=$GhcShortVersion GhcMajVersion=$GhcMajVersion GhcMinVersion=$GhcMinVersion GhcPatchLevel=$GhcPatchLevel
fi

AC_SUBST(GhcShortVersion)

# Check for ghc-pkg
AC_PATH_PROGS(GHCPKG,ghc-pkg)
if test -z "$GHCPKG"; then
   if test "$BINARY" = NO; then
      echo Without ghc-pkg you cannot compile UniForM.
      GOAHEAD=NO
   fi
fi

# Try to find GHC's documents.
AC_ARG_WITH(
   ghc-docs,
   AC_HELP_STRING(
      [--with-ghc-html],
      [Set to pathname of Glasgow Haskell's html directory]
      ),
   GHCHTML=$withval,
   GHCHTML=${GHCHTML}
   )
# Try to guess from GHC's installation directory.
echo $GHCHTML $GHC
if test -z "$GHCHTML" -a -n "$GHC"; then
   # The funny things bracketted by @ signs are
   # "Quadrigraphs" which autoconf translates to square brackets.
   GHCHTML=`$GHC --print-libdir | sed -e 's+/lib/\(@<:@^/@:>@*\)$+/share/doc/ghc+g'`
fi

if test -d $GHCHTML/libraries
then
   echo Glasgow Haskell HTML documents found in $GHCHTML
else
   GHCHTML=
   echo No HTML documents or Haddock files found for GHC.
   echo This will mean ./runhaddock.sh cannot insert links within
   echo Haddocked versions of our modules to GHC modules.
fi 

AC_SUBST(GHCHTML)

#
# Check for wish
AC_PATH_PROGS(WISH,wish wish_8.0 tixwish,NOTFOUND)
if test -z "$WISH"; then
    echo "Without wish you can run hardly any of UniForM's graphical programs"
    WISH="/usr/bin/wish"
fi
#
# Check for daVinci
if test -n "$UDG_HOME"; then
   UDG=$UDG_HOME/bin/uDrawGraph
   echo Assuming uDrawGraph to be $UDG because of variable \$UDG_HOME
   AC_SUBST(uDrawGraph)
else if test -n "$DAVINCIHOME"; then
   UDG=$DAVINCIHOME/bin/daVinci
   echo Assuming DAVINCI=$UDG because of variable \$DAVINCIHOME
   AC_SUBST(daVinci)
   else
   AC_PATH_PROGS(UDG,uDrawGraph daVinci)
   fi
fi

if test -z "$UDG"; then
    echo "Without uDrawGraph you cannot run any of UniForM's graph-drawing programs"
    UDG="uDrawGraph"
fi

# for compatibility
DAVINCI=$UDG
AC_SUBST(DAVINCI)

#
# 
# Check for gnu tar and for zip
AC_PATH_PROGS(TAR,gtar tar)
if test -z "$TAR"; then
   echo "Without GNU tar you cannot build tarballs."
fi
#
AC_PATH_PROGS(ZIP,zip)
if test -z "$ZIP"; then
   echo "Without zip you cannot build zip archives."
fi
#
# Check for haddock
AC_PATH_PROGS(HADDOCK,haddock)
echo haddock is used for providing browsable versions of the HTk sources.
#
# Check for gfind, cp, ln and mkdir
AC_PATH_PROGS(GFIND,gfind,find)
AC_PATH_PROGS(CP,cp)
AC_PATH_PROGS(LN,ln)
AC_PATH_PROGS(MKDIR,mkdir)
#
# Use Berkeley Data Base if user specifies it or if it's
# available in the environment variable BDBDIR or in the default location.
AC_ARG_WITH(
   bdb,
   AC_HELP_STRING(
      [--with-bdb],
      [Set to pathname where Berkeley DB is installed]
      ),
   BDBDIR=$withval,
   BDBDIR=${BDBDIR:-/usr/local/BerkeleyDB.4.0}
   )
if test -r "$BDBDIR/lib/libdb.a"; then
   echo Using Berkeley DB installed in $BDBDIR
else
   echo No Berkeley DB installation found or specified with --with-bdb.
   echo You will not be able to compile the simpledb or types directories.
   echo \(But it should still be possible to compile htk.\)
   BDBDIR=

   if test $BUILDAMOUNT -gt 70
   then
      BUILDAMOUNT=70
   fi
fi
AC_SUBST(BDBDIR)
#
# Check for xemacs and gnuclient
AC_PATH_PROGS(XEMACS,xemacs)
AC_PATH_PROGS(GNUCLIENT,gnuclient)
#
# Check for latex, xdvi, acroread, lp
AC_PATH_PROGS(LATEX,latex)
AC_PATH_PROGS(XDVI,xdvi)
AC_PATH_PROGS(ACROREAD,acroread)
AC_PATH_PROGS(LP,lp)
# 
# Find the current installation of MMiSSLaTeX
AC_ARG_WITH(
   MMiSS-LaTeX,
   AC_HELP_STRING(
      [--with-MMiSS-LaTeX],
      [Set to pathname where MMiSS-LaTeX is installed]
      ),
   MMISSLATEXDIR=$withval,
   MMISSLATEXDIR=${MMISSLATEXDIR:-/usr/local/MMISS/tex}
   )
MISSLATEX=$MMISSLATEXDIR/bin/misslatex
if test -r "$MISSLATEX"; then
   echo Using MMiSS LaTeX found at $MISSLATEX 
else
   MISSLATEX="$TOP/mmiss/scripts/notfound MMiSS-LaTeX "
fi
AC_SUBST(MISSLATEX)

# Check for mkdtemp in stdlib (used by mmiss/test/MainTempDir.c).  We can get 
# along without it, and have to on Solaris.
AC_CHECK_FUNCS(mkdtemp,HAVE_MKDTEMP=1,HAVE_MKDTEMP=0)
AC_SUBST(HAVE_MKDTEMP)

#
# Construct the default options, used by 
# util/default_options.c
# The format of these is described in util/WBFiles.hs.  We make a feeble
# and incomplete attempt to escape any funny characters so we can
# put them into util/default_options.c.  Note that we have to double-double
# escape \ characters, once for sed and once for sh.
DEFAULTOPTIONS=`echo --uni-wish:$WISH --uni-daVinci:$UDG --uni-daVinciIcons:$TOP/database/icons --uni-gnuclient:$GNUCLIENT --uni-top:$TOP | sed -e 's+\\\\+\\\\\\\\+g
s+"+\\\\"+g'`
AC_SUBST(DEFAULTOPTIONS)

# test for HaXml package
HAXMLVERSION=`$GHCPKG field HaXml version`
if test "$HAXMLVERSION" = "version: 1.13.2"; then
   echo $HAXMLVERSION of HaXml found
else
   echo Without HaXml-1.13.2 you cannot compile UniForM.
   GOAHEAD=NO
fi  

# Find all files Makefile.in
MAKEFILES=`$GFIND . -name "Makefile.in" | sed -e "s/.in$//"`
#
if test "$GOAHEAD" = YES; then
   if test "$BINARY" = NO; then
      echo Compiling MainFixFileNames
      # We need to remove the interface file as otherwise strange things
      # happen if the format changes.
      rm -rf mk/Main.hi
      $GHC mk/MainFixFileNames.hs -o mk/FixFileNames
   fi

   FIXFILENAMES=$PWD/mk/FixFileNames

   # Find out what GHC thinks this directory is (this will not necessarily be 
   # the same as TOP on Windows).  This string is escaped.
   GHCTOP=`echo '#PWD' | $FIXFILENAMES`
   echo GHC calls this directory $GHCTOP
   AC_SUBST(GHCTOP)

   echo ''
   echo '------------------- Configuration Successful ------------------'
   if test "$BINARY" = NO; then
      SCRIPTS="mmiss/test/runWorkbench mmiss/test/runServer posixutil/daVinci.debug posixutil/wish.debug mmiss/scripts/domisslatex mmiss/scripts/doxdvi mmiss/scripts/doacroread mmiss/scripts/dolp server/runServer util/CompileFlags.hs runhaddock.sh mk/cabal-ghc mk/cabal-ghc-pkg"
   else
      SCRIPTS="mmiss/test/runWorkbench mmiss/test/rerunWorkbench mmiss/scripts/domisslatex mmiss/scripts/doxdvi mmiss/scripts/doacroread mmiss/scripts/dolp posixutil/daVinci.debug posixutil/wish.debug"
   fi

   # Not all SCRIPTS exist in some binary releases.  Filter out those that
   # don't.
   SCRIPTS1=`for i in $SCRIPTS;do if test -e $i.in ; then echo $i; fi;done`

   OTHERFILES="mk/machinedep.mk mk/boilerplate.mk includes/uni-config.h" 


   AC_OUTPUT($MAKEFILES $SCRIPTS1 $OTHERFILES)
   chmod +x $SCRIPTS1

   if test "$BINARY" = NO
   then   
      echo Setting up packages and export packages file as empty
      # The echo prints []; the funny things bracketted by @ signs are
      # "Quadrigraphs" which express this so.  Normal square brackets
      # won't work since they have a special meaning for autoconf.
      echo @<:@@:>@ >uni-package.conf
      echo @<:@@:>@ >uni-package.conf.export
      mk/FixFileNames <uni-package.options | $GHCPKG --package-conf=uni-package.conf register -      

      GHCPKGOPTS=--force
      
      $GHCPKG $GHCPKGOPTS --package-conf=uni-package.conf.export register - <uni-package.options
      if test -n "$BDBDIR"; then
         ((cd $BDBDIR/lib;$FIXFILENAMES BDBLIB) | (cd $BDBDIR/include;$FIXFILENAMES BDBINCLUDE)) <bdb-package.options | $GHCPKG --package-conf=uni-package.conf register - 
         ((cd $BDBDIR/lib;$FIXFILENAMES BDBLIB) | (cd $BDBDIR/include;$FIXFILENAMES BDBINCLUDE)) <bdb-package.options | $GHCPKG --package-conf=uni-package.conf.export register - 
      fi

      GMAKESHORT=`basename $GMAKE`

      echo To compile UniForM type \"${GMAKESHORT} cabal\"
      # only cabal works for ghc-6.8.1 currently
   fi
fi


