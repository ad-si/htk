dnl == autoconf source for mmiss-checker ==
AC_INIT()
#
# Setting the program name 
PROGRAM=mmiss-checker
AC_SUBST(PROGRAM)

# Set GOAHEAD to NO prevent modifying Makefiles and so on.
GOAHEAD=YES
#
# Get  the top of the build tree
TOP=`pwd`
AC_SUBST(TOP)
#
# Are we running Windows or Unix?
case `uname` in
   CYGWIN* ) WINDOWS=1 ; OSTITLE=windows ; OSSHORT=Win ;;
   MINGW32* ) WINDOWS=1 ; OSTITLE=windows ; OSSHORT=Win ;;
   Linux* ) WINDOWS=0 ; OSTITLE=linux ; OSSHORT=Linux ;;
   SunOS* ) WINDOWS=0 ; OSTITLE=solaris ; OSSHORT=SunOS ;;
   FreeBSD* ) WINDOWS=0 ; OSTITLE=freebsd; OSSHORT=FreeBSD  ;;
   * ) WINDOWS=0; OSTITLE=unknown ; OSSHORT=Unknown ;;
esac
AC_SUBST(WINDOWS)
AC_SUBST(OSTITLE)
AC_SUBST(OSSHORT)
#
# Find a C compiler.
# AC_PATH_PROGS(CC,gcc cc,)
#
# Find a GNU linker
# AC_PATH_PROGS(LD,gnu-ld gld ld,)
# $LD --version
#
# Check for gmake
AC_PATH_PROGS(GMAKE,gmake,make)
$GMAKE --version
#
echo "You need GNU make to compile $PROGRAM".
#
# Use Lisp binary if user specifies it or if it's
# available in the environment variable LISP or in the default location.
AC_ARG_WITH(
   lisp,
   AC_HELP_STRING(
      [--with-lisp],
      [Set to Lisp binary]
      ),
   CMULISP=$withval,
   CMULISP=${LISP:-/usr/local/bin/lisp}
   )
if test -r "$CMULISP"; then
   echo Using Lisp at $CMULISP
else
   echo No Lisp installation found or specified with --with-lisp.
   echo You will not be able to compile the checker.
   GOAHEAD=NO
   CMULISP=
fi

if test "$GOAHEAD" == "YES"; then  
   AC_SUBST(CMULISP)
   AC_OUTPUT(Makefile)
   GMAKESHORT=`basename $GMAKE`
   echo Successfully created the Makefile.
   echo To compile the program type \"${GMAKESHORT} all\".
else
   echo Creation of Makefile failed. 
fi


