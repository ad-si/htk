module MMiSSFiles where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

newtype FileTypes = FileTypes [FileTypes_]              deriving (Eq,Show)
data FileTypes_ = FileTypes_FileType FileType
                | FileTypes_Menu Menu
                deriving (Eq,Show)
data FileType = FileType
    { fileTypeTag :: String
    , fileTypeMenu :: (Maybe String)
    , fileTypeColour :: (Maybe String)
    , fileTypeShape :: (Maybe String)
    } deriving (Eq,Show)
data Menu = Menu Menu_Attrs [Menu_]
          deriving (Eq,Show)
data Menu_Attrs = Menu_Attrs
    { menuId :: String
    , menuTitle :: (Maybe String)
    } deriving (Eq,Show)
data Menu_ = Menu_DisplayVariants DisplayVariants
           | Menu_SelectVariants SelectVariants
           | Menu_EditPermissions EditPermissions
           | Menu_ViewAllPermissions ViewAllPermissions
           | Menu_SubMenu SubMenu
           | Menu_Separator Separator
           | Menu_Command Command
           deriving (Eq,Show)
data DisplayVariants = DisplayVariants
    { displayVariantsTitle :: (Defaultable String)
    } deriving (Eq,Show)
data SelectVariants = SelectVariants
    { selectVariantsTitle :: (Defaultable String)
    } deriving (Eq,Show)
data EditPermissions = EditPermissions
    { editPermissionsTitle :: (Defaultable String)
    } deriving (Eq,Show)
data ViewAllPermissions = ViewAllPermissions
    { viewAllPermissionsTitle :: (Defaultable String)
    } deriving (Eq,Show)
data SubMenu = SubMenu
    { subMenuMenu :: String
    } deriving (Eq,Show)
data Separator = Separator              deriving (Eq,Show)
data Command = Command
    { commandTitle :: String
    , commandConfirm :: (Maybe String)
    , commandCommand :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent FileTypes where
    fromElem (CElem (Elem "fileTypes" [] c0):rest) =
        (\(a,ca)->
           (Just (FileTypes a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (FileTypes a) =
        [CElem (Elem "fileTypes" [] (concatMap toElem a))]
instance XmlContent FileTypes_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (FileTypes_FileType a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (FileTypes_Menu a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (FileTypes_FileType a) = toElem a
    toElem (FileTypes_Menu a) = toElem a
instance XmlContent FileType where
    fromElem (CElem (Elem "fileType" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "fileType" (toAttrs as) [])]
instance XmlAttributes FileType where
    fromAttrs as =
        FileType
          { fileTypeTag = definiteA fromAttrToStr "fileType" "tag" as
          , fileTypeMenu = possibleA fromAttrToStr "menu" as
          , fileTypeColour = possibleA fromAttrToStr "colour" as
          , fileTypeShape = possibleA fromAttrToStr "shape" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "tag" (fileTypeTag v)
        , maybeToAttr toAttrFrStr "menu" (fileTypeMenu v)
        , maybeToAttr toAttrFrStr "colour" (fileTypeColour v)
        , maybeToAttr toAttrFrStr "shape" (fileTypeShape v)
        ]
instance XmlContent Menu where
    fromElem (CElem (Elem "menu" as c0):rest) =
        (\(a,ca)->
           (Just (Menu (fromAttrs as) a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Menu as a) =
        [CElem (Elem "menu" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Menu_Attrs where
    fromAttrs as =
        Menu_Attrs
          { menuId = definiteA fromAttrToStr "menu" "id" as
          , menuTitle = possibleA fromAttrToStr "title" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "id" (menuId v)
        , maybeToAttr toAttrFrStr "title" (menuTitle v)
        ]
instance XmlContent Menu_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (Menu_DisplayVariants a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (Menu_SelectVariants a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,rest) -> (Just (Menu_EditPermissions a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,rest) -> (Just (Menu_ViewAllPermissions a), rest)
                                (_,_) ->
                                        case (fromElem c0) of
                                        (Just a,rest) -> (Just (Menu_SubMenu a), rest)
                                        (_,_) ->
                                                case (fromElem c0) of
                                                (Just a,rest) -> (Just (Menu_Separator a), rest)
                                                (_,_) ->
                                                        case (fromElem c0) of
                                                        (Just a,rest) -> (Just (Menu_Command a), rest)
                                                        (_,_) ->
                                                            (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Menu_DisplayVariants a) = toElem a
    toElem (Menu_SelectVariants a) = toElem a
    toElem (Menu_EditPermissions a) = toElem a
    toElem (Menu_ViewAllPermissions a) = toElem a
    toElem (Menu_SubMenu a) = toElem a
    toElem (Menu_Separator a) = toElem a
    toElem (Menu_Command a) = toElem a
instance XmlContent DisplayVariants where
    fromElem (CElem (Elem "displayVariants" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "displayVariants" (toAttrs as) [])]
instance XmlAttributes DisplayVariants where
    fromAttrs as =
        DisplayVariants
          { displayVariantsTitle = defaultA fromAttrToStr "Display Variants" "title" as
          }
    toAttrs v = catMaybes
        [ defaultToAttr toAttrFrStr "title" (displayVariantsTitle v)
        ]
instance XmlContent SelectVariants where
    fromElem (CElem (Elem "selectVariants" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "selectVariants" (toAttrs as) [])]
instance XmlAttributes SelectVariants where
    fromAttrs as =
        SelectVariants
          { selectVariantsTitle = defaultA fromAttrToStr "Select Variants" "title" as
          }
    toAttrs v = catMaybes
        [ defaultToAttr toAttrFrStr "title" (selectVariantsTitle v)
        ]
instance XmlContent EditPermissions where
    fromElem (CElem (Elem "editPermissions" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "editPermissions" (toAttrs as) [])]
instance XmlAttributes EditPermissions where
    fromAttrs as =
        EditPermissions
          { editPermissionsTitle = defaultA fromAttrToStr "Edit Permissions" "title" as
          }
    toAttrs v = catMaybes
        [ defaultToAttr toAttrFrStr "title" (editPermissionsTitle v)
        ]
instance XmlContent ViewAllPermissions where
    fromElem (CElem (Elem "viewAllPermissions" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "viewAllPermissions" (toAttrs as) [])]
instance XmlAttributes ViewAllPermissions where
    fromAttrs as =
        ViewAllPermissions
          { viewAllPermissionsTitle = defaultA fromAttrToStr "View All Permissions" "title" as
          }
    toAttrs v = catMaybes
        [ defaultToAttr toAttrFrStr "title" (viewAllPermissionsTitle v)
        ]
instance XmlContent SubMenu where
    fromElem (CElem (Elem "subMenu" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "subMenu" (toAttrs as) [])]
instance XmlAttributes SubMenu where
    fromAttrs as =
        SubMenu
          { subMenuMenu = definiteA fromAttrToStr "subMenu" "menu" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "menu" (subMenuMenu v)
        ]
instance XmlContent Separator where
    fromElem (CElem (Elem "separator" [] []):rest) =
        (Just Separator, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem Separator =
        [CElem (Elem "separator" [] [])]
instance XmlContent Command where
    fromElem (CElem (Elem "command" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "command" (toAttrs as) [])]
instance XmlAttributes Command where
    fromAttrs as =
        Command
          { commandTitle = definiteA fromAttrToStr "command" "title" as
          , commandConfirm = possibleA fromAttrToStr "confirm" as
          , commandCommand = definiteA fromAttrToStr "command" "command" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "title" (commandTitle v)
        , maybeToAttr toAttrFrStr "confirm" (commandConfirm v)
        , toAttrFrStr "command" (commandCommand v)
        ]


{-Done-}
