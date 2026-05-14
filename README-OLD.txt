This directory contains Uni, the central part of the UniForM workbench.
The source of Uni and the MMiSS Workbench is in the following directories,
which are listed in the order they are compiled.


   directory    |  contents

   mk           |  Makefile magic which set up default rules and variables
   util         |  Miscellanous Haskell functions used all over Uni
   events       |  Events, providing a way for concurrent events to
                |  communicated
   reactor      |  Miscellaneous things for running processes and tools
   posixutil    |  Various utilities that only work under Posix.
   htk          |  This is an encapsulation of Tcl/Tk in Haskell
                |  See htk/examples for a few short introductory examples.
                |  The htk code is divided among several sub-directories.
   server       |  Stuff for client-server relationships
   graphs       |  General framework for shared graphs.
   davinci      |  This is an encapsulation of uDra(Graph) in Haskell
   simpledb     |  A very simple database system.
   imports      |
   types        |  Repository
   emacs        |  Contains the code for editing MMiSS objects with XEmacs
   mmiss/parser |  (Un)Parses LaTeX sources for MMiSS objects.
   mmiss        |  General code for MMiSS objects.
   mmiss/api    |  Code for interfacing MMiSS via an API

Requirements for compiling UniForM
----------------------------------

At the moment, the Uni sources and the MMiSS workbench have been
successfully compiled on Linux/x86. Microsoft Windows XP (running
MinGW),  Sparc Solaris, and MacOS Darwin should work, too.
The HTk part also used to work on FreeBSD.

The following things are specifically required if you want to compile
everything.  If you just want HTk, you only need (1)-(3).  If you just want
HTk + uDraw(Graph), you only need (1)-(4).

(1) Glasgow Haskell. http://www.haskell.org/ghc/

    This release specifically requires version 6.10.x or 6.12.x

(2) A standard Unix-ish environment.  On
    Linux/Solaris/MacOS Darwin/FreeBSD this is a given.  On Windows there is
    a choice between MinGW and Cygwin, both of which provide you with
    Unix-like shells, gcc, libraries, and so on.  For compiling the Workbench,
    I recommend MinGW, which seems to be simpler than Cygwin, and friendlier
    about working with other (non MinGW/Cygwin) programs.  MinGW is also, by
    the way, capable of compiling GHC.  You can get it from

       http://www.mingw.org

    You will need at least MSYS, MinGW and the MSYS Developer Tool Kit.
    You will probably also want TclTK (for (3).

    However Cygwin (http://www.cygwin.com) may well work as well.

(3) Tcl/Tk.  htk uses the (very common) "wish" program for all its basic user
    interaction.  Typing "wish" at your command prompt should produce
    a small empty window called "wish" if you have it (typing "exit" will
    then allow you to escape).  If you don't have it, you should be able to
    find somewhere to download it from

       http://www.tcl.tk

    If you want another look-and-feel, try tixwish
    ( http://tix.sourceforge.net ).

(4) uDraw(Graph) formerly daVinci.  This is what the Workbench uses to
    display graphs. You can get version 3.1.1 from

       http://www.informatik.uni-bremen.de/uDrawGraph

(5) The Berkeley Database is used for all storage by the repository.
    You can get this from

       http://www.sleepycat.com

    I recommend using the option --disable-shared when you compile it,
    since that will prevent you needing to the BDB shared library around
    when you run executables.

    Thus in BDB's directory [BDB source]/build-unix, you might use
    the command

       ../dist/configure --disable-shared

    configure has other useful, sometimes essential, options, which
    you can find out by type

       ../dist/configure --help

    For example if you are using MinGW, you need the --enable-mingw option;
    if you want the compiled code to go somewhere other than the standard
    system locations (for example, because you don't have administrator
    permissions on your machine) you will need --prefix, and so on.

    Versions of BDB including 4.1.25, 4.2.52, and 4.3.21 have all been
    found to work with the Workbench.  You do not need a version with
    cryptography.

    The Workbench still needs to be able to FIND the BDB files, if they
    are not installed in the standard place.  You can tell it either
    by setting the environment variable BDBDIR, or by giving UniForM's
    ./configure script an option --with-bdb=[directory path].

(6) XEmacs.  This is used directly by the Workbench for editing objects.
    At the moment I am using XEmacs 21.4 patch 8, but I've had no
    trouble with other versions which are slightly earlier or slightly later.
    Do not, however, expect normal GNU Emacs to work, as it won't.
    (This is mainly because we make heavy use of XEmacs' extent mechanism.
    GNU Emacs has a similar mechanism, but it has a different interface.
    Anyone wishing to convert uni/emacs/extents.el to GNU Emacs is welcome
    to try.)   You can get XEmacs from

       http://www.xemacs.org

    The Workbench communicates with XEmacs using the "gnuserv".
    This needs to be enabled from XEmacs. You can do this by running
    the Lisp command "(gnuserv-start)" within XEmacs.  Alternatively you
    can add the same command to your .xemacs/init.el so that it is run
    every time XEmacs start.

    Annoyingly, there are some releases of XEmacs which do not include
    gnuserv, in particular the Netinstaller version of XEmacs currently
    available for Windows.  I have however been able to get an XEmacs working
    with gnuserv on Windows by compiling the Netinstaller version with a
    version of gnuclient/gnuserv compiled from source.  For the latter, I
    followed the instructions for compiling XEmacs on cygwin, but instead
    of compiling all the XEmacs sources I just went to the xemacs-21.4.5/
    directory, ran ./configure (following carefully the instructions here

       http://www.xemacs.org/Documentation/21.5/html/xemacs-faq_3.html#SEC76

    ), and then went into lib-src directory and typed "make".  This saves
    you the bother of compiling all the rest of XEmacs, which you don't want,
    since the Netinstaller version has them.  Make sure when you've done that
    that the cygwin1.dll file is where you keep DLLs on your Windows system
    (such as C:\WINDOWS\SYSTEM), that gnuserv.exe is where your Netinstaller
    version of XEmacs can find it (look for mmencode.exe and put it in the
    same directory), and that gnuclient.exe is on your PATH.  Yes, as I said
    it's annoying.

(7) To run the Workbench and TeX the output, you will need MMiSS TeX.  This is
    *not* currently in the public domain.  However assuming you have it,
    and have configure'd it, UniForM again needs to know where to find it.
    It will guess /usr/local/MMISS/tex, but if MMiSS-LaTeX is not there
    you can specify its location with the environment variable MMISSLATEXDIR,
    or by a ./configure option --with-MMiSS-LaTeX=[MMiSS-LaTeX directory]).
    (The name of this directory will most likely end "MMISS/tex".)

    If you don't have MMiSS TeX, you can still compile and run the Workbench,
    but won't be able to TeX anything.

Of course you also need the sources to Uni!  They should be with this
README file, but in case they are not, or in case you want to update
your copies, here are details.  The Uni sources are kept in a
publicly accessible SVN repository at svn-agbkb.informatik.uni-bremen.de.

So for example to get a complete set of the latest version of the sources you
type (in bash or sh)

  svn co https://svn-agbkb.informatik.uni-bremen.de/uni/trunk uni

This will create a directory uni in your current directory containing
the latest sources for uni.

What To Do Before Compiling
----------------------------

1) Do you want a debugging version of Uni?  By default you won't
   get it. (grep for isDebug)

2) How much do you want to compile?  The default is everything,
   but you can give configure the option --enable-HTk, if all you
   want to compile is HTk or --enable-Het if you just want to
   compile the things needed for Hets (HTk + uDraw(Graph)).
   If ./configure can't find the files it needs it may decide not
   to compile everything anyway; for example if it can't find
   a Berkeley Data Base it will just compile HTk and uDrawGraph.

3) The programs (ghc, wish, uDrawGraph, gnuclient) should
   be in your PATH  when you run your applications
   (./configure just checks them).
   If you want to compile everything, you will also
   need to tell ./configure where to find the Berkeley Data Base, see
   (5) in the "Requirements section".

Compiling
---------

Once you've done all that, the following commands are supposed to do
it, assuming that you are in the same directory as this README file
(that is, at the top of the Uni distribution).  Here, and elsewhere in this
document, I shall use "gmake" to mean "whatever command corresponds to
GNU make on your system".  Other versions of make, such as that called "make"
on Solaris, do not work.

# Configure the Makefiles
./configure
# Compile Uni
gmake cabal

Compiling the Emacs Lisp files
------------------------------

You can compile the Emacs lisp files by starting the XEmacs server
(see instructions above) and then typing
gmake objsemacs


Other Make targets for UniForM developers
-----------------------------------------

The
   ./configure
   gmake cabal
sequence described above is just if you want to build the whole thing from
scratch.  Once you've done this, you may want to modify a file and then
recompile.

The Make targets described in this section all work in any directory of uni
that contains a Makefile.  If you use then in a subdirectory, they will only
apply to that subdirectory.  For example, if you have modified a file, you
can do
   gmake cabal
in the directory containing that file

As described in the next section "Makefiles for uni", source files are
divided into package files, test files and main files.  You can use
   gmake testcabal
to compile just the files you are interested in.  Of course, to compile
the test and main files, you should already have compiled the cabal packages.

Makefiles for uni
-----------------

In each subdirectory of uni there is a file Makefile.in.  If you want to
create another new package you will need to copy this file.  It normally has
the following format:

---- cut here --------------
SRCS = foo1.hs foo2.hs foo3.lhs . . .
PACKAGE = [package name]
PACKAGES = package1 package2 . . .

include @TOP@/mk/boilerplate.mk
---- cut here --------------

Thus we have a list of variable settings (which may be in any order),
then we include the general Make magic.

SRCS contains all the Haskell source files in this directory.
   The Makefiles interpret these specially according to name.
   Files with names of the type "Test*.hs" are test programs,
      should contain a module Main, and will be compiled to an executable
      with name "test*", or "test*.exe" on Windows, for example "TestFoo.hs"
      will go to "Foo" or "Foo.exe"
   Files with names "Main*.hs" are just the same
      as files "Test*.hs", except that the final executable
      is just called "*", so "MainFoo.hs" goes to "Foo".
   All other files are package files.  They should contain a module with the
      name of the file (so the file Foo.hs must contain the module Foo; the
      module subdir/Bar.hs must contain the module Bar).

PACKAGE is the name you want to give to the package contained in this
   directory.  Normally this name begins with "uni-". This should be
   specified if and only if the directory contains library files.
PACKAGES is the list of packages you will need.  This includes
   GHC packages (net and posix for example) and also UniForM packages
   (like uni-htk).  You only have to give direct dependencies here; for
   example uni-htk imports uni-util, so if you specify uni-htk, you do not
   need uni-util.

The following variables also occur occasionally:
SUBDIRS specifies a list of subdirectories of this directory also containing
   Makefiles, into which we should recurse.
SRCSC is like SRCS, but expects a list of C files.

Basically SUBDIRS is currently used to create all cabal packages.

After you've modified a Makefile.in, you should run
   ./configure
(from the top directory)
followed by
   gmake cabal
to recompile the cabal packages.
(Use "ghc-pkg unregister" to remove older or corrupt versions)


Other Specific Issues for Windows (untested)
---------------------------------

It is possible to specify the file names
directly, by setting the environment variables UNIWISH, UNIDAVINCI and
UNIGNUCLIENT to the location of the executables.  If they are in your PATH,
you can in fact just set them to "wish", "uDrawGraph" and "gnuclient".

More information about Make targets
-----------------------------------

Make targets are those names you put after "gmake", like "cabal"
in the above instructions.

Most Make targets can be used in any directory of the UniForM source
containing a makefile, not just the top one.

"clean" deletes all object files

There are some other targets, but you will have to look in mk/suffix.mk
and Makefile.in to see what they are.

Other Makefile Hacking
----------------------

The file mk/local.mk (mentioned above) is included in every Makefile if
present, and can be used to change the options and provide extra targets.

Conclusion
----------

Please report problems to chr.maeder@web.de.

Good luck!
