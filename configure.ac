dnl == autoconf source for the UniForM workbench ==
dnl some details stolen from GHC configure.in file.
# Check that sources are available
AC_INIT(util/Debug.hs)
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
# Check for ghc
AC_PATH_PROGS(GHC,ghc)
if test -z "$GHC"; then
   echo "Without a recent version of Glasgow Haskell you cannot compile UniForm"
   GOAHEAD=NO
else
   UNI_GHC_VERSION([GhcVersion],[GhcMajVersion],[GhcMinVersion],[GhcPatchLevel])
  AC_SUBST(GhcVersion)
  AC_SUBST(GhcMajVersion)
  AC_SUBST(GhcMinVersion)
  AC_SUBST(GhcPatchLevel)

  echo GhcVersion=$GhcVersion GhcMajVersion=$GhcMajVersion GhcMinVersion=$GhcMinVersion GhcPatchLevel=$GhcPatchLevel
fi

# Check for ghc-pkg
AC_PATH_PROGS(GHCPKG,ghc-pkg)
if test -z "$GHCPKG"; then
   echo Without ghc-pkg you cannot compile UniForM.
   GOAHEAD=NO
fi

#
# Check for wish
AC_PATH_PROGS(WISH,wish wish_8.0,NOTFOUND)
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
fi
AC_SUBST(BDBDIR)   
# 
# Construct the default options, used by 
# util/default_options.c
# The format of these is described in util/WBFiles.hs.  We make a feeble
# and incomplete attempt to escape any funny characters so we can
# put them into util/default_options.c.  Note that we have to double-double
# escape \ characters, once for sed and once for sh.
DEFAULTOPTIONS=`echo --uni-wish:$WISH --uni-daVinci:$DAVINCI --uni-daVinciIcons:$TOP/database/icons --uni-top:$TOP | sed -e 's+\\\\+\\\\\\\\+g
s+"+\\\\"+g'`
AC_SUBST(DEFAULTOPTIONS)

# Find all files Makefile.in
MAKEFILES=`$GFIND . -name "Makefile.in" | sed -e "s/.in$//"`
#
if test "$GOAHEAD" = YES; then
   echo Compiling MainFixFileNames
   # We need to remove the interface file as otherwise strange things
   # happen if the format changes.
   rm -rf mk/Main.hi
   $GHC mk/MainFixFileNames.hs -o mk/FixFileNames

   # Find out what GHC thinks this directory is (this will not necessarily be 
   # the same as TOP on Windows).  This string is escaped.
   GHCTOP=`echo '#PWD' | mk/FixFileNames`
   echo GHC calls this directory $GHCTOP
   AC_SUBST(GHCTOP)

   echo ''
   echo '------------------- Configuration Successful ------------------'
   AC_OUTPUT(mk/machinedep.mk mk/boilerplate.mk includes/config.h bdb-package.options $MAKEFILES)

   echo Setting up packages and export packages file as empty
   # The echo prints []; the funny things bracketted by @ signs are
   # "Quadrigraphs" which express this so.  Normal square brackets
   # won't work since they have a special meaning for autoconf.
   echo @<:@@:>@ >uni-package.conf
   echo @<:@@:>@ >uni-package.conf.export
   mk/FixFileNames <uni-package.options | $GHCPKG --config-file uni-package.conf --add-package
   
   $GHCPKG --config-file uni-package.conf.export --add-package <uni-package.options

   if test -n "$BDBDIR"; then
      $GHCPKG --config-file uni-package.conf --add-package <bdb-package.options
   fi

   echo 'To compile UniForM type "gmake boot" followed by "gmake packages"'
   echo 'If you also want the test programs do "gmake all" instead of "gmake packages"'
fi

