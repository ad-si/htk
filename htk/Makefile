THIS	   = htk
IMPS	   = */*.hi

HCINC      = 

LIBS       = $(REACTORDIR)/libreactor.a\
	     $(CONCDIR)/libconcurrency.a\
	     $(UTILDIR)/libutil.a\

LIB        = libhtk.a

SRCS       = \
         resources/GUIValue.hs\
	 resources/Geometry.hs\
	 resources/GUIEvent.hs\
	 resources/Cursor.hs\
	 resources/Colour.hs\
	 resources/Font.hs\
	 resources/Resources.hs\
	 kernel/GUIWish.hs\
	 kernel/GUIObjectKind.hs\
	 kernel/GUIObjectName.hs\
	 kernel/GUIMethods.hs\
	 kernel/GUIObject.hs\
	 kernel/GUIState.hs\
	 kernel/GUIRealise.hs\
	 kernel/GUIBaseClasses.hs\
	 kernel/GUIInteraction.hs\
	 kernel/GUICore.hs\
	 containers/Packer.hs\
	 containers/Toplevel.hs\
	 containers/Window.hs\
	 containers/Frame.hs\
	 containers/Box.hs\
	 components/Slider.hs\
	 components/Index.hs\
	 components/Selection.hs\
	 components/ICursor.hs\
	 components/BitMap.hs\
	 components/Image.hs\
	 components/Icon.hs\
	 devices/Mouse.hs\
	 devices/Keyboard.hs\
	 devices/Printer.hs\
	 devices/Screen.hs\
	 devices/Bell.hs\
	 devices/XSelection.hs\
	 menuitems/Menu.hs\
	 menuitems/MenuItem.hs\
	 menuitems/Indicator.hs\
	 menuitems/Button.hs\
	 menuitems/RadioButton.hs\
	 menuitems/CheckButton.hs\
	 menuitems/MenuButton.hs\
	 menuitems/PulldownMenu.hs\
	 menuitems/Separator.hs\
	 widgets/ScrollBar.hs\
	 widgets/Canvas.hs\
	 widgets/Entry.hs\
	 widgets/Label.hs\
	 widgets/Message.hs\
	 widgets/OptionMenu.hs\
	 widgets/Scale.hs\
	 widgets/ListBox.hs\
	 widgets/Editor.hs\
	 widgets/Space.hs\
	 canvasitems/CanvasItem.hs\
	 canvasitems/CanvasItemAux.hs\
	 canvasitems/CanvasTag.hs\
	 canvasitems/Arc.hs\
	 canvasitems/Line.hs\
	 canvasitems/Oval.hs\
	 canvasitems/Polygon.hs\
	 canvasitems/Rectangle.hs\
	 canvasitems/EmbeddedCanvasWin.hs\
	 canvasitems/TextItem.hs\
	 canvasitems/BitMapItem.hs\
	 canvasitems/ImageItem.hs\
	 textitems/TextTag.hs\
	 textitems/EmbeddedTextWin.hs\
	 textitems/Mark.hs\
	 toplevel/GUIIntrinsics.hs\
	 toplevel/HTk.hs\
	 toolkit/ScrollBox.hs\
	 toolkit/LabelBox.hs\
	 toolkit/ComboBox.hs\
	 toolkit/SelectBox.hs\
	 toolkit/ModalDialog.hs\
	 toolkit/DialogWin.hs\
	 toolkit/IconBar.hs\
	 toolkit/RadioGroup.hs\
	 toolkit/Prompt.hs\
	 toolkit/SpinButton.hs\
	 toolkit/InputForm.hs\
	 toolkit/InputWin.hs\
	 toolkit/PromptWin.hs\
	 toolkit/LogWin.hs\
	 toolkit/MatrixBox.hs\
      
SRCSH      = 

SRCSC      = 

PGMS       = 

PGM_OBJS   =  

EXTRA_DEPS =  


EXTRA_HC_OPTIONS += -Onot -H30M

include top/mk/boilerplate.mk

HCDIRS := resources:canvasitems:kernel:containers:menuitems:toolkit:components:toplevel:widgets:devices:textitems:$(HCDIRS)

cleanhtk:
	rm -f pgm
	rm -f *.hi
	rm -f *.o
	rm -f *%
	rm -f */pgm
	rm -f */*.o
	rm -f */*.hi
	rm -f */*%
	rm -f yum.log canvas.ps
	rm -f libguikernel.a


# DO NOT DELETE: Beginning of Haskell dependencies
resources/GUIValue.o : resources/GUIValue.hs
resources/GUIValue.o : top/util/Computation.hi
resources/GUIValue.o : top/util/Debug.hi
resources/Geometry.o : resources/Geometry.hs
resources/Geometry.o : resources/GUIValue.hi
resources/Geometry.o : top/util/Debug.hi
resources/GUIEvent.o : resources/GUIEvent.hs
resources/GUIEvent.o : top/reactor/InterActor.hi
resources/GUIEvent.o : resources/GUIValue.hi
resources/GUIEvent.o : top/util/Dynamics.hi
resources/GUIEvent.o : top/util/ExtendedPrelude.hi
resources/GUIEvent.o : resources/Geometry.hi
resources/GUIEvent.o : top/util/Debug.hi
resources/Cursor.o : resources/Cursor.hs
resources/Cursor.o : resources/GUIValue.hi
resources/Cursor.o : resources/Colour.hi
resources/Cursor.o : top/util/Debug.hi
resources/Colour.o : resources/Colour.hs
resources/Colour.o : resources/GUIValue.hi
resources/Colour.o : top/util/Debug.hi
resources/Font.o : resources/Font.hs
resources/Font.o : resources/GUIValue.hi
resources/Font.o : top/util/ExtendedPrelude.hi
resources/Font.o : top/util/Debug.hi
resources/Resources.o : resources/Resources.hs
resources/Resources.o : resources/Colour.hi
resources/Resources.o : resources/Cursor.hi
resources/Resources.o : resources/Font.hi
resources/Resources.o : resources/Geometry.hi
resources/Resources.o : resources/GUIValue.hi
resources/Resources.o : resources/GUIEvent.hi
resources/Resources.o : top/util/Dynamics.hi
resources/Resources.o : top/util/Debug.hi
kernel/GUIWish.o : kernel/GUIWish.hs
kernel/GUIWish.o : top/reactor/SIM.hi
kernel/GUIWish.o : top/reactor/ChildProcess.hi
kernel/GUIWish.o : top/util/Object.hi
kernel/GUIWish.o : resources/Resources.hi
kernel/GUIWish.o : top/util/WBFiles.hi
kernel/GUIWish.o : top/util/Debug.hi
kernel/GUIObjectKind.o : kernel/GUIObjectKind.hs
kernel/GUIObjectKind.o : resources/Resources.hi
kernel/GUIObjectKind.o : top/util/Debug.hi
kernel/GUIObjectName.o : kernel/GUIObjectName.hs
kernel/GUIObjectName.o : top/util/Object.hi
kernel/GUIObjectName.o : resources/Resources.hi
kernel/GUIObjectName.o : top/util/Debug.hi
kernel/GUIMethods.o : kernel/GUIMethods.hs
kernel/GUIMethods.o : top/util/Object.hi
kernel/GUIMethods.o : resources/Resources.hi
kernel/GUIMethods.o : resources/GUIValue.hi
kernel/GUIMethods.o : resources/GUIEvent.hi
kernel/GUIMethods.o : kernel/GUIWish.hi
kernel/GUIMethods.o : kernel/GUIObjectKind.hi
kernel/GUIMethods.o : kernel/GUIObjectName.hi
kernel/GUIMethods.o : top/util/Debug.hi
kernel/GUIObject.o : kernel/GUIObject.hs
kernel/GUIObject.o : top/concurrency/Concurrency.hi
kernel/GUIObject.o : resources/Resources.hi
kernel/GUIObject.o : kernel/GUIObjectName.hi
kernel/GUIObject.o : kernel/GUIObjectKind.hi
kernel/GUIObject.o : kernel/GUIMethods.hi
kernel/GUIObject.o : top/util/Maybes.hi
kernel/GUIObject.o : top/util/Object.hi
kernel/GUIObject.o : top/util/Debug.hi
kernel/GUIState.o : kernel/GUIState.hs
kernel/GUIState.o : top/reactor/SIM.hi
kernel/GUIState.o : top/util/ExtendedPrelude.hi
kernel/GUIState.o : resources/Resources.hi
kernel/GUIState.o : kernel/GUIWish.hi
kernel/GUIState.o : kernel/GUIMethods.hi
kernel/GUIState.o : kernel/GUIObject.hi
kernel/GUIState.o : kernel/GUIObjectName.hi
kernel/GUIState.o : kernel/GUIObjectKind.hi
kernel/GUIState.o : top/util/Debug.hi
kernel/GUIRealise.o : kernel/GUIRealise.hs
kernel/GUIRealise.o : top/concurrency/Concurrency.hi
kernel/GUIRealise.o : resources/Resources.hi
kernel/GUIRealise.o : kernel/GUIObject.hi
kernel/GUIRealise.o : kernel/GUIState.hi
kernel/GUIRealise.o : kernel/GUIWish.hi
kernel/GUIRealise.o : top/util/Debug.hi
kernel/GUIBaseClasses.o : kernel/GUIBaseClasses.hs
kernel/GUIBaseClasses.o : top/reactor/SIM.hi
kernel/GUIBaseClasses.o : top/util/Dynamics.hi
kernel/GUIBaseClasses.o : kernel/GUIState.hi
kernel/GUIBaseClasses.o : kernel/GUIRealise.hi
kernel/GUIBaseClasses.o : resources/Resources.hi
kernel/GUIBaseClasses.o : kernel/GUIObject.hi
kernel/GUIBaseClasses.o : top/util/Debug.hi
kernel/GUIInteraction.o : kernel/GUIInteraction.hs
kernel/GUIInteraction.o : top/reactor/SIM.hi
kernel/GUIInteraction.o : top/util/Dynamics.hi
kernel/GUIInteraction.o : kernel/GUIState.hi
kernel/GUIInteraction.o : resources/Resources.hi
kernel/GUIInteraction.o : kernel/GUIObject.hi
kernel/GUIInteraction.o : kernel/GUIBaseClasses.hi
kernel/GUIInteraction.o : top/util/Debug.hi
kernel/GUICore.o : kernel/GUICore.hs
kernel/GUICore.o : top/reactor/SIM.hi
kernel/GUICore.o : kernel/GUIBaseClasses.hi
kernel/GUICore.o : kernel/GUIInteraction.hi
kernel/GUICore.o : resources/Resources.hi
kernel/GUICore.o : resources/Geometry.hi
kernel/GUICore.o : resources/Colour.hi
kernel/GUICore.o : resources/Cursor.hi
kernel/GUICore.o : resources/Font.hi
kernel/GUICore.o : kernel/GUIState.hi
kernel/GUICore.o : kernel/GUIRealise.hi
kernel/GUICore.o : resources/GUIValue.hi
kernel/GUICore.o : resources/GUIEvent.hi
kernel/GUICore.o : kernel/GUIObject.hi
kernel/GUICore.o : containers/Window.hi
kernel/GUICore.o : containers/Packer.hi
kernel/GUICore.o : top/util/Debug.hi
containers/Packer.o : containers/Packer.hs
containers/Packer.o : top/concurrency/Concurrency.hi
containers/Packer.o : resources/Resources.hi
containers/Packer.o : kernel/GUIObject.hi
containers/Packer.o : kernel/GUIState.hi
containers/Packer.o : kernel/GUIRealise.hi
containers/Packer.o : kernel/GUIBaseClasses.hi
containers/Packer.o : top/util/Debug.hi
containers/Toplevel.o : containers/Toplevel.hs
containers/Toplevel.o : top/concurrency/Concurrency.hi
containers/Toplevel.o : resources/Resources.hi
containers/Toplevel.o : kernel/GUIObject.hi
containers/Toplevel.o : kernel/GUIState.hi
containers/Toplevel.o : kernel/GUIRealise.hi
containers/Toplevel.o : kernel/GUIBaseClasses.hi
containers/Toplevel.o : kernel/GUIInteraction.hi
containers/Toplevel.o : containers/Packer.hi
containers/Toplevel.o : top/util/Debug.hi
containers/Window.o : containers/Window.hs
containers/Window.o : top/reactor/SIM.hi
containers/Window.o : resources/Resources.hi
containers/Window.o : kernel/GUIObject.hi
containers/Window.o : kernel/GUIState.hi
containers/Window.o : kernel/GUIRealise.hi
containers/Window.o : kernel/GUIBaseClasses.hi
containers/Window.o : kernel/GUIInteraction.hi
containers/Window.o : containers/Packer.hi
containers/Window.o : containers/Toplevel.hi
containers/Window.o : top/util/Debug.hi
containers/Frame.o : containers/Frame.hs
containers/Frame.o : top/concurrency/Concurrency.hi
containers/Frame.o : kernel/GUICore.hi
containers/Frame.o : containers/Packer.hi
containers/Frame.o : top/util/Debug.hi
containers/Box.o : containers/Box.hs
containers/Box.o : top/concurrency/Concurrency.hi
containers/Box.o : kernel/GUICore.hi
containers/Box.o : containers/Packer.hi
containers/Box.o : containers/Frame.hi
containers/Box.o : top/util/Debug.hi
components/Slider.o : components/Slider.hs
components/Slider.o : top/concurrency/Concurrency.hi
components/Slider.o : kernel/GUICore.hi
components/Slider.o : top/util/Debug.hi
components/Index.o : components/Index.hs
components/Index.o : top/concurrency/Thread.hi
components/Index.o : kernel/GUICore.hi
components/Index.o : top/util/ExtendedPrelude.hi
components/Index.o : top/util/Debug.hi
components/Selection.o : components/Selection.hs
components/Selection.o : top/concurrency/Concurrency.hi
components/Selection.o : kernel/GUICore.hi
components/Selection.o : components/Index.hi
components/Selection.o : top/util/Debug.hi
components/ICursor.o : components/ICursor.hs
components/ICursor.o : top/concurrency/Concurrency.hi
components/ICursor.o : kernel/GUICore.hi
components/ICursor.o : components/Index.hi
components/ICursor.o : top/util/Debug.hi
components/BitMap.o : components/BitMap.hs
components/BitMap.o : top/concurrency/Concurrency.hi
components/BitMap.o : resources/GUIValue.hi
components/BitMap.o : kernel/GUICore.hi
components/BitMap.o : top/util/Debug.hi
components/Image.o : components/Image.hs
components/Image.o : top/concurrency/Concurrency.hi
components/Image.o : kernel/GUICore.hi
components/Image.o : containers/Packer.hi
components/Image.o : top/util/Debug.hi
components/Icon.o : components/Icon.hs
components/Icon.o : top/concurrency/Concurrency.hi
components/Icon.o : kernel/GUICore.hi
components/Icon.o : components/BitMap.hi
components/Icon.o : top/util/Debug.hi
devices/Mouse.o : devices/Mouse.hs
devices/Mouse.o : top/reactor/SIM.hi
devices/Mouse.o : kernel/GUICore.hi
devices/Mouse.o : top/util/Debug.hi
devices/Keyboard.o : devices/Keyboard.hs
devices/Keyboard.o : top/reactor/SIM.hi
devices/Keyboard.o : kernel/GUICore.hi
devices/Keyboard.o : top/util/Debug.hi
devices/Printer.o : devices/Printer.hs
devices/Printer.o : top/concurrency/Thread.hi
devices/Printer.o : kernel/GUICore.hi
devices/Printer.o : top/util/Debug.hi
devices/Screen.o : devices/Screen.hs
devices/Screen.o : top/concurrency/Thread.hi
devices/Screen.o : kernel/GUICore.hi
devices/Screen.o : top/util/Debug.hi
devices/Bell.o : devices/Bell.hs
devices/Bell.o : top/concurrency/Concurrency.hi
devices/Bell.o : kernel/GUICore.hi
devices/Bell.o : top/util/Debug.hi
devices/XSelection.o : devices/XSelection.hs
devices/XSelection.o : top/reactor/SIM.hi
devices/XSelection.o : kernel/GUICore.hi
devices/XSelection.o : components/Index.hi
devices/XSelection.o : components/Selection.hi
devices/XSelection.o : devices/Screen.hi
devices/XSelection.o : top/util/Debug.hi
menuitems/Menu.o : menuitems/Menu.hs
menuitems/Menu.o : top/reactor/SIM.hi
menuitems/Menu.o : top/util/Dynamics.hi
menuitems/Menu.o : kernel/GUICore.hi
menuitems/Menu.o : components/Image.hi
menuitems/Menu.o : containers/Packer.hi
menuitems/Menu.o : components/BitMap.hi
menuitems/Menu.o : top/util/Debug.hi
menuitems/MenuItem.o : menuitems/MenuItem.hs
menuitems/MenuItem.o : top/concurrency/Concurrency.hi
menuitems/MenuItem.o : kernel/GUICore.hi
menuitems/MenuItem.o : top/reactor/EventLoop.hi
menuitems/MenuItem.o : components/Image.hi
menuitems/MenuItem.o : components/BitMap.hi
menuitems/MenuItem.o : containers/Packer.hi
menuitems/MenuItem.o : top/util/Debug.hi
menuitems/Indicator.o : menuitems/Indicator.hs
menuitems/Indicator.o : top/concurrency/Concurrency.hi
menuitems/Indicator.o : kernel/GUICore.hi
menuitems/Indicator.o : top/reactor/EventLoop.hi
menuitems/Indicator.o : components/Image.hi
menuitems/Indicator.o : components/BitMap.hi
menuitems/Indicator.o : containers/Packer.hi
menuitems/Indicator.o : menuitems/MenuItem.hi
menuitems/Indicator.o : top/util/Debug.hi
menuitems/Button.o : menuitems/Button.hs
menuitems/Button.o : top/reactor/SIM.hi
menuitems/Button.o : kernel/GUICore.hi
menuitems/Button.o : components/Image.hi
menuitems/Button.o : components/BitMap.hi
menuitems/Button.o : containers/Packer.hi
menuitems/Button.o : menuitems/MenuItem.hi
menuitems/Button.o : menuitems/Menu.hi
menuitems/Button.o : top/util/Debug.hi
menuitems/RadioButton.o : menuitems/RadioButton.hs
menuitems/RadioButton.o : top/reactor/SIM.hi
menuitems/RadioButton.o : kernel/GUICore.hi
menuitems/RadioButton.o : components/Image.hi
menuitems/RadioButton.o : components/BitMap.hi
menuitems/RadioButton.o : containers/Packer.hi
menuitems/RadioButton.o : menuitems/Button.hi
menuitems/RadioButton.o : menuitems/Menu.hi
menuitems/RadioButton.o : menuitems/MenuItem.hi
menuitems/RadioButton.o : menuitems/Indicator.hi
menuitems/RadioButton.o : top/util/Debug.hi
menuitems/CheckButton.o : menuitems/CheckButton.hs
menuitems/CheckButton.o : top/reactor/SIM.hi
menuitems/CheckButton.o : kernel/GUICore.hi
menuitems/CheckButton.o : components/Image.hi
menuitems/CheckButton.o : components/BitMap.hi
menuitems/CheckButton.o : containers/Packer.hi
menuitems/CheckButton.o : menuitems/Button.hi
menuitems/CheckButton.o : menuitems/Menu.hi
menuitems/CheckButton.o : menuitems/MenuItem.hi
menuitems/CheckButton.o : menuitems/Indicator.hi
menuitems/CheckButton.o : top/util/Debug.hi
menuitems/MenuButton.o : menuitems/MenuButton.hs
menuitems/MenuButton.o : top/reactor/SIM.hi
menuitems/MenuButton.o : kernel/GUICore.hi
menuitems/MenuButton.o : menuitems/Button.hi
menuitems/MenuButton.o : components/Image.hi
menuitems/MenuButton.o : components/BitMap.hi
menuitems/MenuButton.o : containers/Packer.hi
menuitems/MenuButton.o : menuitems/Menu.hi
menuitems/MenuButton.o : menuitems/MenuItem.hi
menuitems/MenuButton.o : menuitems/Indicator.hi
menuitems/MenuButton.o : top/util/Debug.hi
menuitems/PulldownMenu.o : menuitems/PulldownMenu.hs
menuitems/PulldownMenu.o : top/concurrency/Concurrency.hi
menuitems/PulldownMenu.o : kernel/GUICore.hi
menuitems/PulldownMenu.o : menuitems/Menu.hi
menuitems/PulldownMenu.o : menuitems/MenuButton.hi
menuitems/PulldownMenu.o : menuitems/MenuItem.hi
menuitems/PulldownMenu.o : menuitems/Button.hi
menuitems/PulldownMenu.o : menuitems/CheckButton.hi
menuitems/PulldownMenu.o : menuitems/RadioButton.hi
menuitems/PulldownMenu.o : top/util/Debug.hi
menuitems/Separator.o : menuitems/Separator.hs
menuitems/Separator.o : top/concurrency/Concurrency.hi
menuitems/Separator.o : kernel/GUICore.hi
menuitems/Separator.o : containers/Packer.hi
menuitems/Separator.o : menuitems/Menu.hi
menuitems/Separator.o : menuitems/MenuItem.hi
menuitems/Separator.o : top/util/Debug.hi
widgets/ScrollBar.o : widgets/ScrollBar.hs
widgets/ScrollBar.o : top/concurrency/Concurrency.hi
widgets/ScrollBar.o : kernel/GUICore.hi
widgets/ScrollBar.o : containers/Packer.hi
widgets/ScrollBar.o : components/Slider.hi
widgets/ScrollBar.o : top/util/Debug.hi
widgets/Canvas.o : widgets/Canvas.hs
widgets/Canvas.o : top/concurrency/Concurrency.hi
widgets/Canvas.o : kernel/GUICore.hi
widgets/Canvas.o : components/Image.hi
widgets/Canvas.o : widgets/ScrollBar.hi
widgets/Canvas.o : devices/Printer.hi
widgets/Canvas.o : top/util/Debug.hi
widgets/Entry.o : widgets/Entry.hs
widgets/Entry.o : top/concurrency/Concurrency.hi
widgets/Entry.o : kernel/GUICore.hi
widgets/Entry.o : resources/GUIValue.hi
widgets/Entry.o : containers/Packer.hi
widgets/Entry.o : widgets/ScrollBar.hi
widgets/Entry.o : components/Index.hi
widgets/Entry.o : components/Selection.hi
widgets/Entry.o : devices/XSelection.hi
widgets/Entry.o : components/ICursor.hi
widgets/Entry.o : top/util/Debug.hi
widgets/Label.o : widgets/Label.hs
widgets/Label.o : top/concurrency/Concurrency.hi
widgets/Label.o : kernel/GUICore.hi
widgets/Label.o : containers/Packer.hi
widgets/Label.o : components/Image.hi
widgets/Label.o : components/BitMap.hi
widgets/Label.o : top/util/Debug.hi
widgets/Message.o : widgets/Message.hs
widgets/Message.o : top/concurrency/Concurrency.hi
widgets/Message.o : kernel/GUICore.hi
widgets/Message.o : containers/Packer.hi
widgets/Message.o : top/util/Debug.hi
widgets/OptionMenu.o : widgets/OptionMenu.hs
widgets/OptionMenu.o : top/concurrency/Concurrency.hi
widgets/OptionMenu.o : kernel/GUICore.hi
widgets/OptionMenu.o : resources/GUIValue.hi
widgets/OptionMenu.o : containers/Packer.hi
widgets/OptionMenu.o : menuitems/Button.hi
widgets/OptionMenu.o : menuitems/MenuItem.hi
widgets/OptionMenu.o : top/util/Debug.hi
widgets/Scale.o : widgets/Scale.hs
widgets/Scale.o : top/concurrency/Concurrency.hi
widgets/Scale.o : kernel/GUICore.hi
widgets/Scale.o : containers/Packer.hi
widgets/Scale.o : components/Slider.hi
widgets/Scale.o : top/reactor/Interaction.hi
widgets/Scale.o : top/util/Debug.hi
widgets/ListBox.o : widgets/ListBox.hs
widgets/ListBox.o : top/concurrency/Concurrency.hi
widgets/ListBox.o : kernel/GUICore.hi
widgets/ListBox.o : containers/Packer.hi
widgets/ListBox.o : widgets/ScrollBar.hi
widgets/ListBox.o : components/Index.hi
widgets/ListBox.o : components/Selection.hi
widgets/ListBox.o : devices/Mouse.hi
widgets/ListBox.o : devices/XSelection.hi
widgets/ListBox.o : top/reactor/Interaction.hi
widgets/ListBox.o : top/util/Debug.hi
widgets/Editor.o : widgets/Editor.hs
widgets/Editor.o : top/concurrency/Concurrency.hi
widgets/Editor.o : kernel/GUICore.hi
widgets/Editor.o : components/BitMap.hi
widgets/Editor.o : widgets/ScrollBar.hi
widgets/Editor.o : components/Index.hi
widgets/Editor.o : components/Selection.hi
widgets/Editor.o : devices/XSelection.hi
widgets/Editor.o : components/ICursor.hi
widgets/Editor.o : devices/Keyboard.hi
widgets/Editor.o : top/util/Debug.hi
widgets/Space.o : widgets/Space.hs
widgets/Space.o : toplevel/HTk.hi
widgets/Space.o : containers/Frame.hi
widgets/Space.o : top/util/Debug.hi
canvasitems/CanvasItem.o : canvasitems/CanvasItem.hs
canvasitems/CanvasItem.o : top/concurrency/Concurrency.hi
canvasitems/CanvasItem.o : kernel/GUICore.hi
canvasitems/CanvasItem.o : components/Image.hi
canvasitems/CanvasItem.o : components/BitMap.hi
canvasitems/CanvasItem.o : widgets/Canvas.hi
canvasitems/CanvasItem.o : top/util/Debug.hi
canvasitems/CanvasItemAux.o : canvasitems/CanvasItemAux.hs
canvasitems/CanvasItemAux.o : top/concurrency/Concurrency.hi
canvasitems/CanvasItemAux.o : kernel/GUICore.hi
canvasitems/CanvasItemAux.o : widgets/Canvas.hi
canvasitems/CanvasItemAux.o : canvasitems/CanvasItem.hi
canvasitems/CanvasItemAux.o : top/util/Debug.hi
canvasitems/CanvasTag.o : canvasitems/CanvasTag.hs
canvasitems/CanvasTag.o : top/concurrency/Concurrency.hi
canvasitems/CanvasTag.o : kernel/GUICore.hi
canvasitems/CanvasTag.o : widgets/Canvas.hi
canvasitems/CanvasTag.o : canvasitems/CanvasItem.hi
canvasitems/CanvasTag.o : canvasitems/CanvasItemAux.hi
canvasitems/CanvasTag.o : top/util/Debug.hi
canvasitems/Arc.o : canvasitems/Arc.hs
canvasitems/Arc.o : top/concurrency/Concurrency.hi
canvasitems/Arc.o : kernel/GUICore.hi
canvasitems/Arc.o : canvasitems/CanvasItem.hi
canvasitems/Arc.o : canvasitems/CanvasTag.hi
canvasitems/Arc.o : canvasitems/CanvasItemAux.hi
canvasitems/Arc.o : top/util/Debug.hi
canvasitems/Line.o : canvasitems/Line.hs
canvasitems/Line.o : top/concurrency/Concurrency.hi
canvasitems/Line.o : kernel/GUICore.hi
canvasitems/Line.o : canvasitems/CanvasItem.hi
canvasitems/Line.o : canvasitems/CanvasTag.hi
canvasitems/Line.o : canvasitems/CanvasItemAux.hi
canvasitems/Line.o : top/util/Debug.hi
canvasitems/Oval.o : canvasitems/Oval.hs
canvasitems/Oval.o : top/concurrency/Concurrency.hi
canvasitems/Oval.o : kernel/GUICore.hi
canvasitems/Oval.o : canvasitems/CanvasItem.hi
canvasitems/Oval.o : canvasitems/CanvasTag.hi
canvasitems/Oval.o : canvasitems/CanvasItemAux.hi
canvasitems/Oval.o : top/util/Debug.hi
canvasitems/Polygon.o : canvasitems/Polygon.hs
canvasitems/Polygon.o : top/concurrency/Concurrency.hi
canvasitems/Polygon.o : kernel/GUICore.hi
canvasitems/Polygon.o : canvasitems/CanvasItem.hi
canvasitems/Polygon.o : canvasitems/CanvasTag.hi
canvasitems/Polygon.o : canvasitems/CanvasItemAux.hi
canvasitems/Polygon.o : top/util/Debug.hi
canvasitems/Rectangle.o : canvasitems/Rectangle.hs
canvasitems/Rectangle.o : top/concurrency/Concurrency.hi
canvasitems/Rectangle.o : kernel/GUICore.hi
canvasitems/Rectangle.o : canvasitems/CanvasItem.hi
canvasitems/Rectangle.o : canvasitems/CanvasTag.hi
canvasitems/Rectangle.o : canvasitems/CanvasItemAux.hi
canvasitems/Rectangle.o : top/util/Debug.hi
canvasitems/EmbeddedCanvasWin.o : canvasitems/EmbeddedCanvasWin.hs
canvasitems/EmbeddedCanvasWin.o : top/concurrency/Concurrency.hi
canvasitems/EmbeddedCanvasWin.o : kernel/GUICore.hi
canvasitems/EmbeddedCanvasWin.o : canvasitems/CanvasItem.hi
canvasitems/EmbeddedCanvasWin.o : canvasitems/CanvasTag.hi
canvasitems/EmbeddedCanvasWin.o : canvasitems/CanvasItemAux.hi
canvasitems/EmbeddedCanvasWin.o : top/util/Debug.hi
canvasitems/TextItem.o : canvasitems/TextItem.hs
canvasitems/TextItem.o : top/concurrency/Concurrency.hi
canvasitems/TextItem.o : kernel/GUICore.hi
canvasitems/TextItem.o : canvasitems/CanvasItem.hi
canvasitems/TextItem.o : canvasitems/CanvasTag.hi
canvasitems/TextItem.o : canvasitems/CanvasItemAux.hi
canvasitems/TextItem.o : top/util/Debug.hi
canvasitems/BitMapItem.o : canvasitems/BitMapItem.hs
canvasitems/BitMapItem.o : top/concurrency/Concurrency.hi
canvasitems/BitMapItem.o : kernel/GUICore.hi
canvasitems/BitMapItem.o : canvasitems/CanvasItem.hi
canvasitems/BitMapItem.o : canvasitems/CanvasTag.hi
canvasitems/BitMapItem.o : canvasitems/CanvasItemAux.hi
canvasitems/BitMapItem.o : components/BitMap.hi
canvasitems/BitMapItem.o : top/util/Debug.hi
canvasitems/ImageItem.o : canvasitems/ImageItem.hs
canvasitems/ImageItem.o : top/concurrency/Concurrency.hi
canvasitems/ImageItem.o : kernel/GUICore.hi
canvasitems/ImageItem.o : canvasitems/CanvasItem.hi
canvasitems/ImageItem.o : canvasitems/CanvasTag.hi
canvasitems/ImageItem.o : canvasitems/CanvasItemAux.hi
canvasitems/ImageItem.o : components/Image.hi
canvasitems/ImageItem.o : top/util/Debug.hi
textitems/TextTag.o : textitems/TextTag.hs
textitems/TextTag.o : top/concurrency/Concurrency.hi
textitems/TextTag.o : kernel/GUICore.hi
textitems/TextTag.o : widgets/Editor.hi
textitems/TextTag.o : components/BitMap.hi
textitems/TextTag.o : components/Selection.hi
textitems/TextTag.o : components/Index.hi
textitems/TextTag.o : top/reactor/Interaction.hi
textitems/TextTag.o : top/util/Debug.hi
textitems/EmbeddedTextWin.o : textitems/EmbeddedTextWin.hs
textitems/EmbeddedTextWin.o : top/concurrency/Concurrency.hi
textitems/EmbeddedTextWin.o : kernel/GUICore.hi
textitems/EmbeddedTextWin.o : widgets/Editor.hi
textitems/EmbeddedTextWin.o : components/Index.hi
textitems/EmbeddedTextWin.o : top/util/Debug.hi
textitems/Mark.o : textitems/Mark.hs
textitems/Mark.o : top/concurrency/Concurrency.hi
textitems/Mark.o : kernel/GUICore.hi
textitems/Mark.o : components/Index.hi
textitems/Mark.o : components/Selection.hi
textitems/Mark.o : widgets/Editor.hi
textitems/Mark.o : top/util/Debug.hi
toplevel/GUIIntrinsics.o : toplevel/GUIIntrinsics.hs
toplevel/GUIIntrinsics.o : top/concurrency/Thread.hi
toplevel/GUIIntrinsics.o : kernel/GUICore.hi
toplevel/GUIIntrinsics.o : kernel/GUIState.hi
toplevel/GUIIntrinsics.o : kernel/GUIBaseClasses.hi
toplevel/GUIIntrinsics.o : containers/Packer.hi
toplevel/GUIIntrinsics.o : components/Image.hi
toplevel/GUIIntrinsics.o : components/BitMap.hi
toplevel/GUIIntrinsics.o : menuitems/Button.hi
toplevel/GUIIntrinsics.o : menuitems/MenuItem.hi
toplevel/GUIIntrinsics.o : menuitems/Indicator.hi
toplevel/GUIIntrinsics.o : kernel/GUIObject.hi
toplevel/GUIIntrinsics.o : kernel/GUIObjectKind.hi
toplevel/GUIIntrinsics.o : top/util/Debug.hi
toplevel/HTk.o : toplevel/HTk.hs
toplevel/HTk.o : top/reactor/SIM.hi
toplevel/HTk.o : top/util/Computation.hi
toplevel/HTk.o : top/concurrency/Event.hi
toplevel/HTk.o : top/reactor/EventLoop.hi
toplevel/HTk.o : top/reactor/InterActor.hi
toplevel/HTk.o : top/reactor/Interaction.hi
toplevel/HTk.o : top/reactor/EventStream.hi
toplevel/HTk.o : top/reactor/SIM.hi
toplevel/HTk.o : kernel/GUIBaseClasses.hi
toplevel/HTk.o : kernel/GUIInteraction.hi
toplevel/HTk.o : containers/Window.hi
toplevel/HTk.o : containers/Packer.hi
toplevel/HTk.o : resources/Geometry.hi
toplevel/HTk.o : resources/Resources.hi
toplevel/HTk.o : resources/Geometry.hi
toplevel/HTk.o : resources/Colour.hi
toplevel/HTk.o : resources/GUIValue.hi
toplevel/HTk.o : resources/GUIEvent.hi
toplevel/HTk.o : kernel/GUIObject.hi
toplevel/HTk.o : kernel/GUIState.hi
toplevel/HTk.o : kernel/GUIRealise.hi
toplevel/HTk.o : kernel/GUICore.hi
toplevel/HTk.o : menuitems/Menu.hi
toplevel/HTk.o : components/Icon.hi
toplevel/HTk.o : containers/Box.hi
toplevel/HTk.o : top/util/Debug.hi
toolkit/ScrollBox.o : toolkit/ScrollBox.hs
toolkit/ScrollBox.o : top/concurrency/Concurrency.hi
toolkit/ScrollBox.o : toplevel/HTk.hi
toolkit/ScrollBox.o : widgets/ScrollBar.hi
toolkit/ScrollBox.o : containers/Frame.hi
toolkit/ScrollBox.o : top/util/Debug.hi
toolkit/LabelBox.o : toolkit/LabelBox.hs
toolkit/LabelBox.o : top/concurrency/Concurrency.hi
toolkit/LabelBox.o : toplevel/HTk.hi
toolkit/LabelBox.o : widgets/Label.hi
toolkit/LabelBox.o : containers/Box.hi
toolkit/LabelBox.o : top/util/Debug.hi
toolkit/ComboBox.o : toolkit/ComboBox.hs
toolkit/ComboBox.o : toplevel/HTk.hi
toolkit/ComboBox.o : widgets/Label.hi
toolkit/ComboBox.o : widgets/Entry.hi
toolkit/ComboBox.o : widgets/ListBox.hi
toolkit/ComboBox.o : widgets/ScrollBar.hi
toolkit/ComboBox.o : toolkit/ScrollBox.hi
toolkit/ComboBox.o : devices/Mouse.hi
toolkit/ComboBox.o : devices/Keyboard.hi
toolkit/ComboBox.o : widgets/Space.hi
toolkit/ComboBox.o : top/reactor/Interaction.hi
toolkit/ComboBox.o : top/util/Debug.hi
toolkit/SelectBox.o : toolkit/SelectBox.hs
toolkit/SelectBox.o : top/concurrency/Concurrency.hi
toolkit/SelectBox.o : toplevel/HTk.hi
toolkit/SelectBox.o : containers/Frame.hi
toolkit/SelectBox.o : menuitems/Button.hi
toolkit/SelectBox.o : widgets/Space.hi
toolkit/SelectBox.o : top/reactor/Interaction.hi
toolkit/SelectBox.o : toplevel/GUIIntrinsics.hi
toolkit/SelectBox.o : top/util/Debug.hi
toolkit/ModalDialog.o : toolkit/ModalDialog.hs
toolkit/ModalDialog.o : top/util/Computation.hi
toolkit/ModalDialog.o : top/reactor/Interaction.hi
toolkit/ModalDialog.o : toplevel/HTk.hi
toolkit/ModalDialog.o : devices/Mouse.hi
toolkit/ModalDialog.o : top/util/Debug.hi
toolkit/ModalDialog.o : top/concurrency/Selective.hi
toolkit/DialogWin.o : toolkit/DialogWin.hs
toolkit/DialogWin.o : top/concurrency/Concurrency.hi
toolkit/DialogWin.o : toplevel/HTk.hi
toolkit/DialogWin.o : widgets/Message.hi
toolkit/DialogWin.o : widgets/Label.hi
toolkit/DialogWin.o : components/BitMap.hi
toolkit/DialogWin.o : menuitems/Separator.hi
toolkit/DialogWin.o : devices/Keyboard.hi
toolkit/DialogWin.o : menuitems/Button.hi
toolkit/DialogWin.o : resources/Font.hi
toolkit/DialogWin.o : widgets/Space.hi
toolkit/DialogWin.o : toolkit/SelectBox.hi
toolkit/DialogWin.o : toolkit/ModalDialog.hi
toolkit/DialogWin.o : top/reactor/Interaction.hi
toolkit/DialogWin.o : top/util/Debug.hi
toolkit/IconBar.o : toolkit/IconBar.hs
toolkit/IconBar.o : top/concurrency/Concurrency.hi
toolkit/IconBar.o : toplevel/HTk.hi
toolkit/IconBar.o : menuitems/Button.hi
toolkit/IconBar.o : menuitems/Separator.hi
toolkit/IconBar.o : components/BitMap.hi
toolkit/IconBar.o : components/Image.hi
toolkit/IconBar.o : top/reactor/Interaction.hi
toolkit/IconBar.o : toplevel/GUIIntrinsics.hi
toolkit/IconBar.o : top/util/Debug.hi
toolkit/RadioGroup.o : toolkit/RadioGroup.hs
toolkit/RadioGroup.o : top/concurrency/Concurrency.hi
toolkit/RadioGroup.o : toplevel/HTk.hi
toolkit/RadioGroup.o : menuitems/RadioButton.hi
toolkit/RadioGroup.o : top/reactor/Interaction.hi
toolkit/RadioGroup.o : top/util/Debug.hi
toolkit/Prompt.o : toolkit/Prompt.hs
toolkit/Prompt.o : toplevel/HTk.hi
toolkit/Prompt.o : widgets/Label.hi
toolkit/Prompt.o : widgets/Entry.hi
toolkit/Prompt.o : top/util/Debug.hi
toolkit/SpinButton.o : toolkit/SpinButton.hs
toolkit/SpinButton.o : toplevel/HTk.hi
toolkit/SpinButton.o : menuitems/Button.hi
toolkit/SpinButton.o : components/BitMap.hi
toolkit/SpinButton.o : top/reactor/Interaction.hi
toolkit/SpinButton.o : top/util/WBFiles.hi
toolkit/SpinButton.o : top/util/Debug.hi
toolkit/InputForm.o : toolkit/InputForm.hs
toolkit/InputForm.o : top/concurrency/Concurrency.hi
toolkit/InputForm.o : toplevel/HTk.hi
toolkit/InputForm.o : toolkit/Prompt.hi
toolkit/InputForm.o : containers/Box.hi
toolkit/InputForm.o : widgets/Label.hi
toolkit/InputForm.o : toolkit/DialogWin.hi
toolkit/InputForm.o : widgets/OptionMenu.hi
toolkit/InputForm.o : toolkit/LabelBox.hi
toolkit/InputForm.o : toolkit/ScrollBox.hi
toolkit/InputForm.o : widgets/Editor.hi
toolkit/InputForm.o : devices/Keyboard.hi
toolkit/InputForm.o : menuitems/Separator.hi
toolkit/InputForm.o : widgets/Space.hi
toolkit/InputForm.o : top/util/Debug.hi
toolkit/InputWin.o : toolkit/InputWin.hs
toolkit/InputWin.o : top/concurrency/Concurrency.hi
toolkit/InputWin.o : toplevel/HTk.hi
toolkit/InputWin.o : widgets/Message.hi
toolkit/InputWin.o : menuitems/Separator.hi
toolkit/InputWin.o : devices/Keyboard.hi
toolkit/InputWin.o : menuitems/Button.hi
toolkit/InputWin.o : resources/Font.hi
toolkit/InputWin.o : widgets/Space.hi
toolkit/InputWin.o : toolkit/SelectBox.hi
toolkit/InputWin.o : toolkit/ModalDialog.hi
toolkit/InputWin.o : toolkit/InputForm.hi
toolkit/InputWin.o : toolkit/DialogWin.hi
toolkit/InputWin.o : top/util/Debug.hi
toolkit/PromptWin.o : toolkit/PromptWin.hs
toolkit/PromptWin.o : top/concurrency/Concurrency.hi
toolkit/PromptWin.o : toplevel/HTk.hi
toolkit/PromptWin.o : widgets/Message.hi
toolkit/PromptWin.o : menuitems/Separator.hi
toolkit/PromptWin.o : devices/Keyboard.hi
toolkit/PromptWin.o : menuitems/Button.hi
toolkit/PromptWin.o : resources/Font.hi
toolkit/PromptWin.o : widgets/Space.hi
toolkit/PromptWin.o : toolkit/SelectBox.hi
toolkit/PromptWin.o : toolkit/ModalDialog.hi
toolkit/PromptWin.o : toolkit/Prompt.hi
toolkit/PromptWin.o : toolkit/DialogWin.hi
toolkit/PromptWin.o : widgets/Entry.hi
toolkit/PromptWin.o : top/concurrency/Future.hi
toolkit/PromptWin.o : top/util/Debug.hi
toolkit/LogWin.o : toolkit/LogWin.hs
toolkit/LogWin.o : top/concurrency/Concurrency.hi
toolkit/LogWin.o : toplevel/HTk.hi
toolkit/LogWin.o : menuitems/Separator.hi
toolkit/LogWin.o : menuitems/Button.hi
toolkit/LogWin.o : menuitems/PulldownMenu.hi
toolkit/LogWin.o : widgets/Space.hi
toolkit/LogWin.o : widgets/Editor.hi
toolkit/LogWin.o : widgets/ScrollBar.hi
toolkit/LogWin.o : toolkit/PromptWin.hi
toolkit/LogWin.o : toolkit/DialogWin.hi
toolkit/LogWin.o : toolkit/ScrollBox.hi
toolkit/LogWin.o : top/reactor/Interaction.hi
toolkit/LogWin.o : top/util/Debug.hi
toolkit/MatrixBox.o : toolkit/MatrixBox.hs
toolkit/MatrixBox.o : top/concurrency/Concurrency.hi
toolkit/MatrixBox.o : toplevel/HTk.hi
toolkit/MatrixBox.o : containers/Box.hi
toolkit/MatrixBox.o : top/util/Debug.hi
# DO NOT DELETE: End of Haskell dependencies
