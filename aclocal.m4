dnl 
dnl Extra autoconf macros for the UniForM.
dnl These are stolen from Glasgow Haskell, by deleting things
dnl we don't need and replacing "FPTOOLS_" by "UNI_" 
dnl
dnl To be a good autoconf citizen, names of local macros have
dnl prefixed with UNI_ to ensure we don't clash
dnl with any pre-supplied autoconf ones.
dnl
dnl
dnl UNI_NOCACHE_CHECK prints a message, then sets the
dnl values of the second argument to the result of running
dnl the commands given by the third. It does not cache its
dnl result, so it is suitable for checks which should be
dnl run every time.
dnl
AC_DEFUN(UNI_NOCACHE_CHECK,
[AC_MSG_CHECKING([$1])
 $3
 AC_MSG_RESULT([$][$2])
])

dnl
dnl UNI_GHC_VERSION(version)
dnl UNI_GHC_VERSION(major, minor [, patchlevel])
dnl UNI_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
AC_DEFUN(UNI_GHC_VERSION,
[UNI_NOCACHE_CHECK([version of ghc], [uni_version_of_ghc],
[${WithGhc-ghc} --version > conftestghc 2>&1
  cat conftestghc >&AC_FD_CC
#Useless Use Of cat award...
  uni_version_of_ghc=`cat conftestghc | sed -n -e 's/, patchlevel *\([[0-9]]\)/.\1/;s/.* version \([[0-9]][[0-9.]]*\).*/\1/p'`
  rm -fr conftest*
  if test "[$]uni_version_of_ghc" = ""
  then
    uni_version_of_ghc='unknown'
  fi
uni_version_of_ghc[_major]=`echo [$]uni_version_of_ghc | sed -e 's/^\([[0-9]]\).*/\1/'`
uni_version_of_ghc[_minor]=`echo [$]uni_version_of_ghc | sed -e 's/^[[0-9]]\.\([[0-9]]*\).*/\1/'`
uni_version_of_ghc[_pl]=`echo [$]uni_version_of_ghc | sed -n -e 's/^[[0-9]]\.[[0-9]]*\.\([[0-9]]*\)/\1/p'`
#
if test "[$]uni_version_of_ghc[_pl]" = ""
then
  uni_version_of_ghc[_all]="[$]uni_version_of_ghc[_major].[$]uni_version_of_ghc[_minor]"
  uni_version_of_ghc[_pl]="0"
else
  uni_version_of_ghc[_all]="[$]uni_version_of_ghc[_major].[$]uni_version_of_ghc[_minor].[$]uni_version_of_ghc[_pl]"
fi
#
ifelse($#, [1], [dnl
[$1]="[$]uni_version_of_ghc[_all]"
], $#, [2], [dnl
[$1]="[$]uni_version_of_ghc[_major]"
[$2]="[$]uni_version_of_ghc[_minor]"
], $#, [3], [dnl
[$1]="[$]uni_version_of_ghc[_major]"
[$2]="[$]uni_version_of_ghc[_minor]"
[$3]="[$]uni_version_of_ghc[_pl]"
], $#, [4], [dnl
[$1]="[$]uni_version_of_ghc[_all]"
[$2]="[$]uni_version_of_ghc[_major]"
[$3]="[$]uni_version_of_ghc[_minor]"
[$4]="[$]uni_version_of_ghc[_pl]"
])
])
])dnl
