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
   Linux* ) WINDOWS=0 ; OSTITLE=linux ;;
   SunOS* ) WINDOWS=0 ; OSTITLE=solaris ;;
   FreeBSD* ) WINDOWS=0 ; OSTITLE=freebsd ;;
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
else
   UNI_GHC_VERSION([GhcVersion],[GhcMajVersion],[GhcMinVersion],[GhcPatchLevel])
  AC_SUBST(GhcVersion)
  AC_SUBST(GhcMajVersion)
  AC_SUBST(GhcMinVersion)
  AC_SUBST(GhcPatchLevel)

GhcShortVersion=$GhcMajVersion$GhcMinVersion
  AC_SUBST(GhcShortVersion)

  echo GhcVersion=$GhcVersion GhcShortVersion=$GhcShortVersion GhcMajVersion=$GhcMajVersion GhcMinVersion=$GhcMinVersion GhcPatchLevel=$GhcPatchLevel
fi

# Check for ghc-pkg
AC_PATH_PROGS(GHCPKG,ghc-pkg)
if test -z "$GHCPKG"; then
   if test "$BINARY" = NO; then
      echo Without ghc-pkg you cannot compile UniForM.
      GOAHEAD=NO
   fi
fi

#
# Check for wish
AC_PATH_PROGS(WISH,wish wish_8.0 tixwish,NOTFOUND)
if test -z "$WISH"; then
    echo "Without wish you can run hardly any of UniForM's graphical programs"
    WISH="/usr/bin/wish"
fi
#
# Check for daVinci
AC_PATH_PROGS(DAVINCI,daVinci)
if test -z "$DAVINCI"; then
    echo "Without daVinci you cannot run any of UniForM's graph-drawing programs"
    DAVINCI="/usr/bin/davinci"
fi
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
# Check for hdoc
AC_PATH_PROGS(HDOC,hdoc)
echo hdoc is used for providing browsable versions of the HTk sources.
#
# Check for gfind, cp, ln and mkdir
AC_PATH_PROGS(GFIND,gfind,find)
AC_PATH_PROGS(CP,cp)
AC_PATH_PROGS(LN,ln)
AC_PATH_PROGS(MKDIR,mkdir)
#
WORK_AROUND_BDB_LINUX_BUG=0
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

   if test "$OSTITLE" = "linux"; then
      echo Working around BDB Linux bug.
      echo This means you will get some extra messages from 
      echo MMiSSWorkbench about '(1)' "malloc: using debugging hooks"
      echo '(2)' invalid free pointers.  But hopefully it will work.
      WORK_AROUND_BDB_LINUX_BUG=1
   fi
else
   echo No Berkeley DB installation found or specified with --with-bdb.
   echo You will not be able to compile the simpledb or types directories.
   echo \(But it should still be possible to compile htk.\)
   BDBDIR=
fi
AC_SUBST(BDBDIR)
AC_SUBST(WORK_AROUND_BDB_LINUX_BUG)
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
#
# Decide whether to compile in HaXml or not.
if test $GhcShortVersion -gt 504
then
   HAXML=HaXml
   HAXMLINT=1
else
   HAXML=""
   HAXMLINT=0
fi
AC_SUBST(HAXML)
AC_SUBST(HAXMLINT)

#
# Construct the default options, used by 
# util/default_options.c
# The format of these is described in util/WBFiles.hs.  We make a feeble
# and incomplete attempt to escape any funny characters so we can
# put them into util/default_options.c.  Note that we have to double-double
# escape \ characters, once for sed and once for sh.
DEFAULTOPTIONS=`echo --uni-wish:$WISH --uni-daVinci:$DAVINCI --uni-daVinciIcons:$TOP/database/icons --uni-gnuclient:$GNUCLIENT --uni-top:$TOP | sed -e 's+\\\\+\\\\\\\\+g
s+"+\\\\"+g'`
AC_SUBST(DEFAULTOPTIONS)

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

   # Find out what GHC thinks this directory is (this will not necessarily be 
   # the same as TOP on Windows).  This string is escaped.
   GHCTOP=`echo '#PWD' | mk/FixFileNames`
   echo GHC calls this directory $GHCTOP
   AC_SUBST(GHCTOP)

   echo ''
   echo '------------------- Configuration Successful ------------------'
   if test "$BINARY" = NO; then
      SCRIPTS="types/test/runWorkbench types/test/rerunWorkbench mmiss/test/runWorkbench mmiss/test/rerunWorkbench posixutil/daVinci.debug posixutil/wish.debug mmiss/scripts/domisslatex mmiss/scripts/doxdvi mmiss/scripts/doacroread mmiss/scripts/dolp mmiss/scripts/docp"
   else
      SCRIPTS="mmiss/test/runWorkbench mmiss/test/rerunWorkbench mmiss/scripts/domisslatex mmiss/scripts/doxdvi mmiss/scripts/doacroread mmiss/scripts/dolp mmiss/scripts/docp"
   fi

   AC_OUTPUT(mk/machinedep.mk mk/boilerplate.mk includes/config.h bdb-package.options $MAKEFILES $SCRIPTS)
   chmod +x $SCRIPTS

   if test "$BINARY" = NO
   then   
      echo Setting up packages and export packages file as empty
      # The echo prints []; the funny things bracketted by @ signs are
      # "Quadrigraphs" which express this so.  Normal square brackets
      # won't work since they have a special meaning for autoconf.
      echo @<:@@:>@ >uni-package.conf
      echo @<:@@:>@ >uni-package.conf.export
      mk/FixFileNames <uni-package.options | $GHCPKG --config-file uni-package.conf --add-package

      if test -n "$HAXML"
      then
         echo Linking to HaXml sources.
         ${GMAKE} -C $HAXML -r lnsrcs
      fi

      # Use GHCPKG option with --force if necessary
      if test "$GhcMajVersion.$GhcMinVersion" = "5.02"
      then
         GHCPKGOPTS=
      else
         GHCPKGOPTS=--force
      fi
      
      $GHCPKG $GHCPKGOPTS --config-file uni-package.conf.export --add-package <uni-package.options

      if test -n "$BDBDIR"; then
         $GHCPKG --config-file uni-package.conf --add-package <bdb-package.options
      fi

      GMAKESHORT=`basename $GMAKE`

      echo To compile UniForM type \"${GMAKESHORT} boot\" followed by \"${GMAKESHORT} packages\"
      echo If you also want the test programs do \"${GMAKESHORT} all\" instead of \"${GMAKESHORT} packages\"
   fi
fi


