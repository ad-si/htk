LIB        = libdavinci.a

SRCS       =
         DaVinciGraphTerm.hs\
	 DaVinciEvent.hs\
	 DaVinciCore.hs\
	 DaVinciMenu.hs\
	 DaVinciIcon.hs\
	 DaVinciClasses.hs\
	 DaVinciNodeType.hs\
	 DaVinciEdgeType.hs\
	 DaVinciRules.hs\
	 DaVinciGraph.hs\
	 DaVinciNode.hs\
	 DaVinciEdge.hs\
	 DaVinciDragAndDrop.hs\
	 DaVinciSelection.hs\
	 DaVinciView.hs\
	 DaVinciPopupMenu.hs\
	 DaVinci.hs\
	 WidgetTreeView.hs\
	 GraphEditor.hs\

SUBDIRS = test

include top/mk/boilerplate.mk


# DO NOT DELETE: Beginning of Haskell dependencies
DaVinciGraphTerm.o : DaVinciGraphTerm.hs
DaVinciGraphTerm.o : top/htk/resources/GUIValue.hi
DaVinciGraphTerm.o : top/util/Debug.hi
DaVinciEvent.o : DaVinciEvent.hs
DaVinciEvent.o : top/reactor/SIM.hi
DaVinciEvent.o : top/util/Dynamics.hi
DaVinciEvent.o : ./DaVinciGraphTerm.hi
DaVinciEvent.o : top/util/ExtendedPrelude.hi
DaVinciEvent.o : top/util/Debug.hi
DaVinciCore.o : DaVinciCore.hs
DaVinciCore.o : top/reactor/SIM.hi
DaVinciCore.o : top/util/ExtendedPrelude.hi
DaVinciCore.o : top/reactor/ChildProcess.hi
DaVinciCore.o : top/htk/kernel/GUICore.hi
DaVinciCore.o : top/htk/menuitems/MenuItem.hi
DaVinciCore.o : ./DaVinciGraphTerm.hi
DaVinciCore.o : ./DaVinciEvent.hi
DaVinciCore.o : top/util/WBFiles.hi
DaVinciCore.o : top/util/Debug.hi
DaVinciMenu.o : DaVinciMenu.hs
DaVinciMenu.o : top/reactor/SIM.hi
DaVinciMenu.o : top/htk/kernel/GUICore.hi
DaVinciMenu.o : top/htk/toplevel/HTk.hi
DaVinciMenu.o : top/htk/menuitems/Menu.hi
DaVinciMenu.o : top/htk/menuitems/MenuItem.hi
DaVinciMenu.o : top/htk/menuitems/Button.hi
DaVinciMenu.o : top/htk/toplevel/GUIIntrinsics.hi
DaVinciMenu.o : ./DaVinciGraphTerm.hi
DaVinciMenu.o : ./DaVinciCore.hi
DaVinciMenu.o : ./DaVinciEvent.hi
DaVinciMenu.o : top/util/Debug.hi
DaVinciIcon.o : DaVinciIcon.hs
DaVinciIcon.o : top/reactor/SIM.hi
DaVinciIcon.o : top/htk/kernel/GUICore.hi
DaVinciIcon.o : top/htk/toolkit/IconBar.hi
DaVinciIcon.o : ./DaVinciCore.hi
DaVinciIcon.o : top/htk/components/BitMap.hi
DaVinciIcon.o : top/util/Debug.hi
DaVinciClasses.o : DaVinciClasses.hs
DaVinciClasses.o : top/htk/toplevel/HTk.hi
DaVinciClasses.o : top/htk/resources/Font.hi
DaVinciClasses.o : top/htk/canvasitems/Line.hi
DaVinciClasses.o : ./DaVinciCore.hi
DaVinciClasses.o : top/util/Debug.hi
DaVinciNodeType.o : DaVinciNodeType.hs
DaVinciNodeType.o : top/htk/toplevel/HTk.hi
DaVinciNodeType.o : top/htk/kernel/GUICore.hi
DaVinciNodeType.o : top/htk/resources/Font.hi
DaVinciNodeType.o : top/htk/components/BitMap.hi
DaVinciNodeType.o : top/htk/canvasitems/Line.hi
DaVinciNodeType.o : ./DaVinciCore.hi
DaVinciNodeType.o : ./DaVinciGraphTerm.hi
DaVinciNodeType.o : ./DaVinciClasses.hi
DaVinciNodeType.o : ./DaVinciMenu.hi
DaVinciNodeType.o : top/util/Debug.hi
DaVinciEdgeType.o : DaVinciEdgeType.hs
DaVinciEdgeType.o : top/htk/toplevel/HTk.hi
DaVinciEdgeType.o : top/htk/resources/Font.hi
DaVinciEdgeType.o : top/htk/kernel/GUICore.hi
DaVinciEdgeType.o : ./DaVinciCore.hi
DaVinciEdgeType.o : ./DaVinciGraphTerm.hi
DaVinciEdgeType.o : ./DaVinciClasses.hi
DaVinciEdgeType.o : ./DaVinciMenu.hi
DaVinciEdgeType.o : top/util/Debug.hi
DaVinciRules.o : DaVinciRules.hs
DaVinciRules.o : top/util/Computation.hi
DaVinciRules.o : top/htk/kernel/GUICore.hi
DaVinciRules.o : ./DaVinciCore.hi
DaVinciRules.o : ./DaVinciGraphTerm.hi
DaVinciRules.o : ./DaVinciClasses.hi
DaVinciRules.o : ./DaVinciNodeType.hi
DaVinciRules.o : ./DaVinciEdgeType.hi
DaVinciRules.o : top/util/Debug.hi
DaVinciGraph.o : DaVinciGraph.hs
DaVinciGraph.o : top/reactor/SIM.hi
DaVinciGraph.o : top/htk/kernel/GUICore.hi
DaVinciGraph.o : ./DaVinciCore.hi
DaVinciGraph.o : ./DaVinciEvent.hi
DaVinciGraph.o : ./DaVinciGraphTerm.hi
DaVinciGraph.o : ./DaVinciClasses.hi
DaVinciGraph.o : ./DaVinciMenu.hi
DaVinciGraph.o : top/util/Debug.hi
DaVinciNode.o : DaVinciNode.hs
DaVinciNode.o : top/reactor/SIM.hi
DaVinciNode.o : top/htk/kernel/GUICore.hi
DaVinciNode.o : top/htk/components/BitMap.hi
DaVinciNode.o : ./DaVinciCore.hi
DaVinciNode.o : ./DaVinciClasses.hi
DaVinciNode.o : ./DaVinciGraph.hi
DaVinciNode.o : ./DaVinciGraphTerm.hi
DaVinciNode.o : ./DaVinciNodeType.hi
DaVinciNode.o : top/util/Debug.hi
DaVinciEdge.o : DaVinciEdge.hs
DaVinciEdge.o : top/reactor/SIM.hi
DaVinciEdge.o : top/htk/kernel/GUICore.hi
DaVinciEdge.o : ./DaVinciEdgeType.hi
DaVinciEdge.o : ./DaVinciClasses.hi
DaVinciEdge.o : ./DaVinciGraph.hi
DaVinciEdge.o : ./DaVinciNode.hi
DaVinciEdge.o : ./DaVinciCore.hi
DaVinciEdge.o : ./DaVinciGraphTerm.hi
DaVinciEdge.o : top/util/Debug.hi
DaVinciDragAndDrop.o : DaVinciDragAndDrop.hs
DaVinciDragAndDrop.o : top/reactor/SIM.hi
DaVinciDragAndDrop.o : ./DaVinciCore.hi
DaVinciDragAndDrop.o : top/htk/resources/Resources.hi
DaVinciDragAndDrop.o : ./DaVinciEvent.hi
DaVinciDragAndDrop.o : ./DaVinciNode.hi
DaVinciDragAndDrop.o : ./DaVinciEdge.hi
DaVinciDragAndDrop.o : ./DaVinciGraph.hi
DaVinciDragAndDrop.o : top/util/Debug.hi
DaVinciSelection.o : DaVinciSelection.hs
DaVinciSelection.o : top/reactor/SIM.hi
DaVinciSelection.o : top/htk/components/Selection.hi
DaVinciSelection.o : ./DaVinciCore.hi
DaVinciSelection.o : ./DaVinciGraphTerm.hi
DaVinciSelection.o : ./DaVinciNode.hi
DaVinciSelection.o : ./DaVinciEvent.hi
DaVinciSelection.o : ./DaVinciNode.hi
DaVinciSelection.o : ./DaVinciEdge.hi
DaVinciSelection.o : top/util/Debug.hi
DaVinciView.o : DaVinciView.hs
DaVinciView.o : top/reactor/SIM.hi
DaVinciView.o : top/htk/kernel/GUICore.hi
DaVinciView.o : ./DaVinciCore.hi
DaVinciView.o : ./DaVinciGraph.hi
DaVinciView.o : ./DaVinciGraphTerm.hi
DaVinciView.o : ./DaVinciClasses.hi
DaVinciView.o : top/util/Debug.hi
DaVinciPopupMenu.o : DaVinciPopupMenu.hs
DaVinciPopupMenu.o : top/reactor/SIM.hi
DaVinciPopupMenu.o : ./DaVinciCore.hi
DaVinciPopupMenu.o : ./DaVinciEvent.hi
DaVinciPopupMenu.o : ./DaVinciNode.hi
DaVinciPopupMenu.o : ./DaVinciEdge.hi
DaVinciPopupMenu.o : top/htk/menuitems/Menu.hi
DaVinciPopupMenu.o : top/util/Debug.hi
DaVinci.o : DaVinci.hs
DaVinci.o : top/reactor/SIM.hi
DaVinci.o : top/htk/kernel/GUICore.hi
DaVinci.o : ./DaVinciClasses.hi
DaVinci.o : ./DaVinciNodeType.hi
DaVinci.o : ./DaVinciEdgeType.hi
DaVinci.o : ./DaVinciRules.hi
DaVinci.o : ./DaVinciCore.hi
DaVinci.o : ./DaVinciGraphTerm.hi
DaVinci.o : ./DaVinciEvent.hi
DaVinci.o : ./DaVinciGraph.hi
DaVinci.o : ./DaVinciNode.hi
DaVinci.o : ./DaVinciEdge.hi
DaVinci.o : ./DaVinciView.hi
DaVinci.o : ./DaVinciSelection.hi
DaVinci.o : ./DaVinciDragAndDrop.hi
DaVinci.o : ./DaVinciIcon.hi
DaVinci.o : ./DaVinciPopupMenu.hi
DaVinci.o : top/htk/components/BitMap.hi
DaVinci.o : top/util/Debug.hi
WidgetTreeView.o : WidgetTreeView.hs
WidgetTreeView.o : top/htk/toplevel/HTk.hi
WidgetTreeView.o : ./DaVinci.hi
WidgetTreeView.o : top/htk/toplevel/GUIIntrinsics.hi
WidgetTreeView.o : top/util/Debug.hi
GraphEditor.o : GraphEditor.hs
GraphEditor.o : top/htk/toplevel/HTk.hi
GraphEditor.o : ./DaVinci.hi
GraphEditor.o : top/reactor/SIM.hi
GraphEditor.o : top/htk/menuitems/PulldownMenu.hi
GraphEditor.o : top/htk/toolkit/PromptWin.hi
GraphEditor.o : top/htk/toolkit/ScrollBox.hi
GraphEditor.o : top/htk/toolkit/DialogWin.hi
GraphEditor.o : top/htk/toolkit/InputForm.hi
GraphEditor.o : top/htk/toolkit/InputWin.hi
GraphEditor.o : top/htk/toolkit/IconBar.hi
GraphEditor.o : top/util/Debug.hi
# DO NOT DELETE: End of Haskell dependencies
