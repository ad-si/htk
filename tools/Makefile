LIB        = libtools.a

SRCS       = \
         WB.hs\
         WBFiles.hs\
         Diff.hs\
	 Dvips.hs\
	 Grep.hs\
	 HolCSP.hs\
	 HolZ.hs\
	 HugsIntrp.hs\
	 Hugs.hs\
	 Latex.hs\
	 Moby.hs\
	 Tas.hs\
	 TextEdit.hs\
	 XDvi.hs\

SUBDIRS = test

include top/mk/boilerplate.mk

# DO NOT DELETE: Beginning of Haskell dependencies
WB.o : WB.hs
WBFiles.o : WBFiles.hs
Diff.o : Diff.hs
Diff.o : ./WB.hi
Diff.o : top/htk/toolkit/DialogWin.hi
Diff.o : top/htk/toolkit/LogWin.hi
Diff.o : top/reactor/Expect.hi
Diff.o : top/util/Debug.hi
Dvips.o : Dvips.hs
Dvips.o : ./WB.hi
Dvips.o : top/reactor/Expect.hi
Dvips.o : top/htk/toolkit/DialogWin.hi
Dvips.o : top/htk/toolkit/LogWin.hi
Dvips.o : top/htk/widgets/Editor.hi
Dvips.o : top/htk/toolkit/PromptWin.hi
Dvips.o : top/util/Debug.hi
Grep.o : Grep.hs
Grep.o : ./WB.hi
Grep.o : top/reactor/Expect.hi
Grep.o : top/util/Debug.hi
HolCSP.o : HolCSP.hs
HolCSP.o : ./WB.hi
HolCSP.o : top/htk/toolkit/DialogWin.hi
HolCSP.o : top/reactor/Expect.hi
HolCSP.o : top/util/Debug.hi
HolZ.o : HolZ.hs
HolZ.o : ./WB.hi
HolZ.o : top/htk/toolkit/DialogWin.hi
HolZ.o : top/reactor/Expect.hi
HolZ.o : top/util/ExtendedPrelude.hi
HolZ.o : top/util/Debug.hi
HugsIntrp.o : HugsIntrp.hs
HugsIntrp.o : ./WB.hi
HugsIntrp.o : top/reactor/Expect.hi
HugsIntrp.o : top/util/Debug.hi
Hugs.o : Hugs.hs
Hugs.o : ./WB.hi
Hugs.o : top/reactor/EventLoop.hi
Hugs.o : top/htk/toolkit/ScrollBox.hi
Hugs.o : top/htk/widgets/Editor.hi
Hugs.o : top/htk/devices/Keyboard.hi
Hugs.o : top/htk/containers/Frame.hi
Hugs.o : top/htk/menuitems/PulldownMenu.hi
Hugs.o : top/htk/menuitems/MenuItem.hi
Hugs.o : top/htk/menuitems/Separator.hi
Hugs.o : top/htk/menuitems/Button.hi
Hugs.o : top/htk/menuitems/CheckButton.hi
Hugs.o : top/htk/widgets/Space.hi
Hugs.o : top/htk/toolkit/PromptWin.hi
Hugs.o : top/htk/devices/Bell.hi
Hugs.o : top/htk/toolkit/DialogWin.hi
Hugs.o : top/reactor/ChildProcess.hi
Hugs.o : ./WBFiles.hi
Hugs.o : top/reactor/Expect.hi
Hugs.o : top/util/Debug.hi
Latex.o : Latex.hs
Latex.o : ./WB.hi
Latex.o : top/htk/toolkit/DialogWin.hi
Latex.o : top/htk/toolkit/LogWin.hi
Latex.o : top/htk/widgets/Editor.hi
Latex.o : top/htk/toolkit/PromptWin.hi
Latex.o : top/reactor/Interaction.hi
Latex.o : top/util/Debug.hi
Moby.o : Moby.hs
Moby.o : ./WB.hi
Moby.o : top/util/Debug.hi
Tas.o : Tas.hs
Tas.o : ./WB.hi
Tas.o : top/htk/toolkit/DialogWin.hi
Tas.o : top/reactor/Expect.hi
Tas.o : top/util/ExtendedPrelude.hi
Tas.o : top/util/Debug.hi
TextEdit.o : TextEdit.hs
TextEdit.o : ./WB.hi
TextEdit.o : top/util/Debug.hi
XDvi.o : XDvi.hs
XDvi.o : ./WB.hi
XDvi.o : top/htk/toolkit/DialogWin.hi
XDvi.o : top/htk/toolkit/LogWin.hi
XDvi.o : top/htk/widgets/Editor.hi
XDvi.o : top/htk/toolkit/PromptWin.hi
XDvi.o : top/util/Debug.hi
# DO NOT DELETE: End of Haskell dependencies
