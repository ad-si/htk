module MMiSSRequest where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

data Request = RequestConnect Connect
             | RequestCloseServer CloseServer
             | RequestListVersions ListVersions
             | RequestCheckOut CheckOut
             | RequestChangeUserInfo ChangeUserInfo
             | RequestCommitVersion CommitVersion
             | RequestCloseVersion CloseVersion
             | RequestGetObject GetObject
             | RequestPutObject PutObject
             | RequestGetPermissions GetPermissions
             | RequestSetPermissions SetPermissions
             | RequestSetAdminStatus SetAdminStatus
             deriving (Eq,Show)
data Response = Response Messages
                         (Maybe (OneOf12 ConnectResponse CloseServerResponse ListVersionsResponse CheckOutResponse ChangeUserInfoResponse CommitVersionResponse CloseVersionResponse GetObjectResponse PutObjectResponse GetPermissionsResponse SetPermissionsResponse SetAdminStatusResponse))
              deriving (Eq,Show)
data Connect = Connect Connect_Attrs (Maybe ServerRef)
             deriving (Eq,Show)
data Connect_Attrs = Connect_Attrs
    { connectServer :: (Maybe String)
    , connectUser :: (Maybe String)
    , connectPassword :: (Maybe String)
    } deriving (Eq,Show)
newtype ConnectResponse = ConnectResponse ServerRef 		deriving (Eq,Show)
newtype CloseServer = CloseServer ServerRef 		deriving (Eq,Show)
data CloseServerResponse = CloseServerResponse 		deriving (Eq,Show)
newtype ListVersions = ListVersions ServerRef 		deriving (Eq,Show)
newtype ListVersionsResponse = ListVersionsResponse [VersionInfo] 		deriving (Eq,Show)
data CheckOut = CheckOut CheckOut_Attrs ServerRef
                         (Maybe VersionRef)
              deriving (Eq,Show)
data CheckOut_Attrs = CheckOut_Attrs
    { checkOutVersion :: String
    } deriving (Eq,Show)
newtype CheckOutResponse = CheckOutResponse VersionRef 		deriving (Eq,Show)
data ChangeUserInfo = ChangeUserInfo VersionRef UserInfo
                    deriving (Eq,Show)
data ChangeUserInfoResponse = ChangeUserInfoResponse 		deriving (Eq,Show)
data CommitVersion = CommitVersion VersionRef (Maybe UserInfo)
                   deriving (Eq,Show)
data CommitVersionResponse = CommitVersionResponse 		deriving (Eq,Show)
newtype CloseVersion = CloseVersion VersionRef 		deriving (Eq,Show)
data CloseVersionResponse = CloseVersionResponse 		deriving (Eq,Show)
data GetObject = GetObject GetObject_Attrs VersionRef
                           ObjectFullName (Maybe Variants)
               deriving (Eq,Show)
data GetObject_Attrs = GetObject_Attrs
    { getObjectWhat :: (Defaultable GetObject_what)
    , getObjectFormat :: (Defaultable GetObject_format)
    , getObjectRecurse :: (Defaultable GetObject_recurse)
    } deriving (Eq,Show)
data GetObject_what = GetObject_what_locations  | 
                      GetObject_what_everything
                    deriving (Eq,Show)
data GetObject_format = GetObject_format_LaTeX  | 
                        GetObject_format_XML
                      deriving (Eq,Show)
data GetObject_recurse = GetObject_recurse_justThis  | 
                         GetObject_recurse_allIncluded
                       deriving (Eq,Show)
newtype GetObjectResponse = GetObjectResponse Bundle 		deriving (Eq,Show)
data PutObject = PutObject VersionRef ObjectFullName
                           (Maybe PackageId) Bundle
               deriving (Eq,Show)
data PutObjectResponse = PutObjectResponse 		deriving (Eq,Show)
newtype GetPermissions = GetPermissions WhichPermissions 		deriving (Eq,Show)
newtype GetPermissionsResponse = GetPermissionsResponse Permissions 		deriving (Eq,Show)
data SetPermissions = SetPermissions WhichPermissions Permissions
                    deriving (Eq,Show)
data SetPermissionsResponse = SetPermissionsResponse 		deriving (Eq,Show)
data SetAdminStatus = SetAdminStatus SetAdminStatus_Attrs ServerRef
                    deriving (Eq,Show)
data SetAdminStatus_Attrs = SetAdminStatus_Attrs
    { setAdminStatusAction :: SetAdminStatus_action
    } deriving (Eq,Show)
data SetAdminStatus_action = SetAdminStatus_action_claim  | 
                             SetAdminStatus_action_revoke
                           deriving (Eq,Show)
data SetAdminStatusResponse = SetAdminStatusResponse 		deriving (Eq,Show)
data ServerRef = ServerRef
    { serverRefRef :: String
    } deriving (Eq,Show)
data VersionRef = VersionRef
    { versionRefRef :: String
    } deriving (Eq,Show)
newtype Bundle = Bundle [Bundle_] 		deriving (Eq,Show)
data Bundle_ = Bundle_ PackageId File
             deriving (Eq,Show)
data PackageId = PackageId
    { packageIdId :: String
    } deriving (Eq,Show)
data File = File FileLocation (Maybe (OneOf2 FileVariants Files))
          deriving (Eq,Show)
data FileLocation = FileLocation (Maybe ObjectName) ObjectType
                  deriving (Eq,Show)
newtype Files = Files [File] 		deriving (Eq,Show)
newtype FileVariants = FileVariants [FileVariant] 		deriving (Eq,Show)
data FileVariant = FileVariant (Maybe Variants)
                               (Maybe FileContents)
                 deriving (Eq,Show)
data FileContents = FileContents
    { fileContentsDataBlock :: String
    , fileContentsCharType :: (Defaultable FileContents_charType)
    } deriving (Eq,Show)
data FileContents_charType = FileContents_charType_byte  | 
                             FileContents_charType_unicode
                           deriving (Eq,Show)
data VersionInfo = VersionInfo VersionInfo_Attrs UserInfo
                               ServerInfo
                 deriving (Eq,Show)
data VersionInfo_Attrs = VersionInfo_Attrs
    { versionInfoIsPresent :: (Defaultable VersionInfo_isPresent)
    } deriving (Eq,Show)
data VersionInfo_isPresent = VersionInfo_isPresent_present  | 
                             VersionInfo_isPresent_absent
                           deriving (Eq,Show)
data UserInfo = UserInfo UserInfo_Attrs (Maybe Attributes)
              deriving (Eq,Show)
data UserInfo_Attrs = UserInfo_Attrs
    { userInfoLabel :: (Maybe String)
    , userInfoContents :: (Maybe String)
    , userInfoVersion :: (Maybe String)
    , userInfoParents :: (Maybe String)
    } deriving (Eq,Show)
data ServerInfo = ServerInfo
    { serverInfoServerId :: String
    , serverInfoSerialNo :: String
    , serverInfoTimeStamp :: String
    , serverInfoUserId :: String
    } deriving (Eq,Show)
newtype ObjectName = ObjectName String 		deriving (Eq,Show)
data ObjectType = ObjectType
    { objectTypeBaseType :: ObjectType_baseType
    , objectTypeExtType :: (Maybe String)
    , objectTypeExtraType :: (Maybe String)
    } deriving (Eq,Show)
data ObjectType_baseType = ObjectType_baseType_folder  | 
                           ObjectType_baseType_plainFile  |  ObjectType_baseType_mmissFolder
                            |  ObjectType_baseType_mmissObject  | 
                           ObjectType_baseType_mmissFile  |  ObjectType_baseType_mmissPreamble
                            |  ObjectType_baseType_unknownType
                         deriving (Eq,Show)
newtype ObjectFullName = ObjectFullName String 		deriving (Eq,Show)
data WhichPermissions = WhichPermissionsServerRef ServerRef
                      | WhichPermissionsVersionRef_ObjectFullName (VersionRef,ObjectFullName)
                      deriving (Eq,Show)
newtype Permissions = Permissions String 		deriving (Eq,Show)
data Variant = Variant
    { variantKey :: String
    , variantValue :: (Maybe String)
    } deriving (Eq,Show)
newtype Variants = Variants [Variant] 		deriving (Eq,Show)
data Attribute = Attribute
    { attributeKey :: String
    , attributeValue :: String
    } deriving (Eq,Show)
newtype Attributes = Attributes [Attribute] 		deriving (Eq,Show)
data Messages = Messages Messages_Attrs [Messages_]
              deriving (Eq,Show)
data Messages_Attrs = Messages_Attrs
    { messagesStatus :: (Defaultable Messages_status)
    } deriving (Eq,Show)
data Messages_ = Messages_Alert Alert
               | Messages_Error Error
               | Messages_Warning Warning
               | Messages_Message Message
               deriving (Eq,Show)
data Messages_status = Messages_status_success  | 
                       Messages_status_fail  |  Messages_status_panic
                     deriving (Eq,Show)
newtype Alert = Alert String 		deriving (Eq,Show)
newtype Error = Error String 		deriving (Eq,Show)
newtype Warning = Warning String 		deriving (Eq,Show)
newtype Message = Message String 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Request where
    fromElem (CElem (Elem "request" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (RequestConnect a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (RequestCloseServer a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,_) -> (Just (RequestListVersions a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,_) -> (Just (RequestCheckOut a), rest)
                                (_,_) ->
                                        case (fromElem c0) of
                                        (Just a,_) -> (Just (RequestChangeUserInfo a), rest)
                                        (_,_) ->
                                                case (fromElem c0) of
                                                (Just a,_) -> (Just (RequestCommitVersion a), rest)
                                                (_,_) ->
                                                        case (fromElem c0) of
                                                        (Just a,_) -> (Just (RequestCloseVersion a), rest)
                                                        (_,_) ->
                                                                case (fromElem c0) of
                                                                (Just a,_) -> (Just (RequestGetObject a), rest)
                                                                (_,_) ->
                                                                        case (fromElem c0) of
                                                                        (Just a,_) -> (Just (RequestPutObject a), rest)
                                                                        (_,_) ->
                                                                                case (fromElem c0) of
                                                                                (Just a,_) -> (Just (RequestGetPermissions a), rest)
                                                                                (_,_) ->
                                                                                        case (fromElem c0) of
                                                                                        (Just a,_) -> (Just (RequestSetPermissions a), rest)
                                                                                        (_,_) ->
                                                                                                case (fromElem c0) of
                                                                                                (Just a,_) -> (Just (RequestSetAdminStatus a), rest)
                                                                                                (_,_) ->
                                                                                                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (RequestConnect a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestCloseServer a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestListVersions a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestCheckOut a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestChangeUserInfo a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestCommitVersion a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestCloseVersion a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestGetObject a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestPutObject a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestGetPermissions a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestSetPermissions a) = [CElem (Elem "request" [] (toElem a) )]
    toElem (RequestSetAdminStatus a) = [CElem (Elem "request" [] (toElem a) )]
instance XmlContent Response where
    fromElem (CElem (Elem "response" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (Response a b), rest))
           (fromElem ca))
        (definite fromElem "<messages>" "response" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Response a b) =
        [CElem (Elem "response" [] (toElem a ++ maybe [] toElem b))]
instance XmlContent Connect where
    fromElem (CElem (Elem "connect" as c0):rest) =
        (\(a,ca)->
           (Just (Connect (fromAttrs as) a), rest))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Connect as a) =
        [CElem (Elem "connect" (toAttrs as) (maybe [] toElem a))]
instance XmlAttributes Connect_Attrs where
    fromAttrs as =
        Connect_Attrs
          { connectServer = possibleA fromAttrToStr "server" as
          , connectUser = possibleA fromAttrToStr "user" as
          , connectPassword = possibleA fromAttrToStr "password" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "server" (connectServer v)
        , maybeToAttr toAttrFrStr "user" (connectUser v)
        , maybeToAttr toAttrFrStr "password" (connectPassword v)
        ]
instance XmlContent ConnectResponse where
    fromElem (CElem (Elem "connectResponse" [] c0):rest) =
        (\(a,ca)->
           (Just (ConnectResponse a), rest))
        (definite fromElem "<serverRef>" "connectResponse" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ConnectResponse a) =
        [CElem (Elem "connectResponse" [] (toElem a))]
instance XmlContent CloseServer where
    fromElem (CElem (Elem "closeServer" [] c0):rest) =
        (\(a,ca)->
           (Just (CloseServer a), rest))
        (definite fromElem "<serverRef>" "closeServer" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (CloseServer a) =
        [CElem (Elem "closeServer" [] (toElem a))]
instance XmlContent CloseServerResponse where
    fromElem (CElem (Elem "closeServerResponse" [] []):rest) =
        (Just CloseServerResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem CloseServerResponse =
        [CElem (Elem "closeServerResponse" [] [])]
instance XmlContent ListVersions where
    fromElem (CElem (Elem "listVersions" [] c0):rest) =
        (\(a,ca)->
           (Just (ListVersions a), rest))
        (definite fromElem "<serverRef>" "listVersions" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ListVersions a) =
        [CElem (Elem "listVersions" [] (toElem a))]
instance XmlContent ListVersionsResponse where
    fromElem (CElem (Elem "listVersionsResponse" [] c0):rest) =
        (\(a,ca)->
           (Just (ListVersionsResponse a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ListVersionsResponse a) =
        [CElem (Elem "listVersionsResponse" [] (concatMap toElem a))]
instance XmlContent CheckOut where
    fromElem (CElem (Elem "checkOut" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (CheckOut (fromAttrs as) a b), rest))
           (fromElem ca))
        (definite fromElem "<serverRef>" "checkOut" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (CheckOut as a b) =
        [CElem (Elem "checkOut" (toAttrs as) (toElem a ++
                                              maybe [] toElem b))]
instance XmlAttributes CheckOut_Attrs where
    fromAttrs as =
        CheckOut_Attrs
          { checkOutVersion = definiteA fromAttrToStr "checkOut" "version" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "version" (checkOutVersion v)
        ]
instance XmlContent CheckOutResponse where
    fromElem (CElem (Elem "checkOutResponse" [] c0):rest) =
        (\(a,ca)->
           (Just (CheckOutResponse a), rest))
        (definite fromElem "<versionRef>" "checkOutResponse" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (CheckOutResponse a) =
        [CElem (Elem "checkOutResponse" [] (toElem a))]
instance XmlContent ChangeUserInfo where
    fromElem (CElem (Elem "changeUserInfo" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (ChangeUserInfo a b), rest))
           (definite fromElem "<userInfo>" "changeUserInfo" ca))
        (definite fromElem "<versionRef>" "changeUserInfo" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ChangeUserInfo a b) =
        [CElem (Elem "changeUserInfo" [] (toElem a ++ toElem b))]
instance XmlContent ChangeUserInfoResponse where
    fromElem (CElem (Elem "changeUserInfoResponse" [] []):rest) =
        (Just ChangeUserInfoResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem ChangeUserInfoResponse =
        [CElem (Elem "changeUserInfoResponse" [] [])]
instance XmlContent CommitVersion where
    fromElem (CElem (Elem "commitVersion" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (CommitVersion a b), rest))
           (fromElem ca))
        (definite fromElem "<versionRef>" "commitVersion" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (CommitVersion a b) =
        [CElem (Elem "commitVersion" [] (toElem a ++ maybe [] toElem b))]
instance XmlContent CommitVersionResponse where
    fromElem (CElem (Elem "commitVersionResponse" [] []):rest) =
        (Just CommitVersionResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem CommitVersionResponse =
        [CElem (Elem "commitVersionResponse" [] [])]
instance XmlContent CloseVersion where
    fromElem (CElem (Elem "closeVersion" [] c0):rest) =
        (\(a,ca)->
           (Just (CloseVersion a), rest))
        (definite fromElem "<versionRef>" "closeVersion" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (CloseVersion a) =
        [CElem (Elem "closeVersion" [] (toElem a))]
instance XmlContent CloseVersionResponse where
    fromElem (CElem (Elem "closeVersionResponse" [] []):rest) =
        (Just CloseVersionResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem CloseVersionResponse =
        [CElem (Elem "closeVersionResponse" [] [])]
instance XmlContent GetObject where
    fromElem (CElem (Elem "getObject" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (Just (GetObject (fromAttrs as) a b c), rest))
              (fromElem cb))
           (definite fromElem "<objectFullName>" "getObject" ca))
        (definite fromElem "<versionRef>" "getObject" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (GetObject as a b c) =
        [CElem (Elem "getObject" (toAttrs as) (toElem a ++ toElem b ++
                                               maybe [] toElem c))]
instance XmlAttributes GetObject_Attrs where
    fromAttrs as =
        GetObject_Attrs
          { getObjectWhat = defaultA fromAttrToTyp GetObject_what_locations "what" as
          , getObjectFormat = defaultA fromAttrToTyp GetObject_format_LaTeX "format" as
          , getObjectRecurse = defaultA fromAttrToTyp GetObject_recurse_justThis "recurse" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "what" (getObjectWhat v)
        , defaultToAttr toAttrFrTyp "format" (getObjectFormat v)
        , defaultToAttr toAttrFrTyp "recurse" (getObjectRecurse v)
        ]
instance XmlAttrType GetObject_what where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "locations" = Just GetObject_what_locations
            translate "everything" = Just GetObject_what_everything
            translate _ = Nothing
    toAttrFrTyp n GetObject_what_locations = Just (n, str2attr "locations")
    toAttrFrTyp n GetObject_what_everything = Just (n, str2attr "everything")
instance XmlAttrType GetObject_format where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "LaTeX" = Just GetObject_format_LaTeX
            translate "XML" = Just GetObject_format_XML
            translate _ = Nothing
    toAttrFrTyp n GetObject_format_LaTeX = Just (n, str2attr "LaTeX")
    toAttrFrTyp n GetObject_format_XML = Just (n, str2attr "XML")
instance XmlAttrType GetObject_recurse where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "justThis" = Just GetObject_recurse_justThis
            translate "allIncluded" = Just GetObject_recurse_allIncluded
            translate _ = Nothing
    toAttrFrTyp n GetObject_recurse_justThis = Just (n, str2attr "justThis")
    toAttrFrTyp n GetObject_recurse_allIncluded = Just (n, str2attr "allIncluded")
instance XmlContent GetObjectResponse where
    fromElem (CElem (Elem "getObjectResponse" [] c0):rest) =
        (\(a,ca)->
           (Just (GetObjectResponse a), rest))
        (definite fromElem "<bundle>" "getObjectResponse" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (GetObjectResponse a) =
        [CElem (Elem "getObjectResponse" [] (toElem a))]
instance XmlContent PutObject where
    fromElem (CElem (Elem "putObject" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (\(d,cd)->
                    (Just (PutObject a b c d), rest))
                 (definite fromElem "<bundle>" "putObject" cc))
              (fromElem cb))
           (definite fromElem "<objectFullName>" "putObject" ca))
        (definite fromElem "<versionRef>" "putObject" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (PutObject a b c d) =
        [CElem (Elem "putObject" [] (toElem a ++ toElem b ++
                                     maybe [] toElem c ++ toElem d))]
instance XmlContent PutObjectResponse where
    fromElem (CElem (Elem "putObjectResponse" [] []):rest) =
        (Just PutObjectResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem PutObjectResponse =
        [CElem (Elem "putObjectResponse" [] [])]
instance XmlContent GetPermissions where
    fromElem (CElem (Elem "getPermissions" [] c0):rest) =
        (\(a,ca)->
           (Just (GetPermissions a), rest))
        (definite fromElem "<whichPermissions>" "getPermissions" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (GetPermissions a) =
        [CElem (Elem "getPermissions" [] (toElem a))]
instance XmlContent GetPermissionsResponse where
    fromElem (CElem (Elem "getPermissionsResponse" [] c0):rest) =
        (\(a,ca)->
           (Just (GetPermissionsResponse a), rest))
        (definite fromElem "<permissions>" "getPermissionsResponse" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (GetPermissionsResponse a) =
        [CElem (Elem "getPermissionsResponse" [] (toElem a))]
instance XmlContent SetPermissions where
    fromElem (CElem (Elem "setPermissions" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (SetPermissions a b), rest))
           (definite fromElem "<permissions>" "setPermissions" ca))
        (definite fromElem "<whichPermissions>" "setPermissions" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (SetPermissions a b) =
        [CElem (Elem "setPermissions" [] (toElem a ++ toElem b))]
instance XmlContent SetPermissionsResponse where
    fromElem (CElem (Elem "setPermissionsResponse" [] []):rest) =
        (Just SetPermissionsResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem SetPermissionsResponse =
        [CElem (Elem "setPermissionsResponse" [] [])]
instance XmlContent SetAdminStatus where
    fromElem (CElem (Elem "setAdminStatus" as c0):rest) =
        (\(a,ca)->
           (Just (SetAdminStatus (fromAttrs as) a), rest))
        (definite fromElem "<serverRef>" "setAdminStatus" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (SetAdminStatus as a) =
        [CElem (Elem "setAdminStatus" (toAttrs as) (toElem a))]
instance XmlAttributes SetAdminStatus_Attrs where
    fromAttrs as =
        SetAdminStatus_Attrs
          { setAdminStatusAction = definiteA fromAttrToTyp "setAdminStatus" "action" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "action" (setAdminStatusAction v)
        ]
instance XmlAttrType SetAdminStatus_action where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "claim" = Just SetAdminStatus_action_claim
            translate "revoke" = Just SetAdminStatus_action_revoke
            translate _ = Nothing
    toAttrFrTyp n SetAdminStatus_action_claim = Just (n, str2attr "claim")
    toAttrFrTyp n SetAdminStatus_action_revoke = Just (n, str2attr "revoke")
instance XmlContent SetAdminStatusResponse where
    fromElem (CElem (Elem "setAdminStatusResponse" [] []):rest) =
        (Just SetAdminStatusResponse, rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem SetAdminStatusResponse =
        [CElem (Elem "setAdminStatusResponse" [] [])]
instance XmlContent ServerRef where
    fromElem (CElem (Elem "serverRef" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "serverRef" (toAttrs as) [])]
instance XmlAttributes ServerRef where
    fromAttrs as =
        ServerRef
          { serverRefRef = definiteA fromAttrToStr "serverRef" "ref" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "ref" (serverRefRef v)
        ]
instance XmlContent VersionRef where
    fromElem (CElem (Elem "versionRef" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "versionRef" (toAttrs as) [])]
instance XmlAttributes VersionRef where
    fromAttrs as =
        VersionRef
          { versionRefRef = definiteA fromAttrToStr "versionRef" "ref" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "ref" (versionRefRef v)
        ]
instance XmlContent Bundle where
    fromElem (CElem (Elem "bundle" [] c0):rest) =
        (\(a,ca)->
           (Just (Bundle a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Bundle a) =
        [CElem (Elem "bundle" [] (concatMap toElem a))]
instance XmlContent Bundle_ where
    fromElem c0 =
        case (\(a,ca)->
                (\(b,cb)->
                   (a,b,cb))
                (fromElem ca))
             (fromElem c0) of
        (Just a,Just b,rest) -> (Just (Bundle_ a b), rest)
        (_,_,_) ->
            (Nothing, c0)
    toElem (Bundle_ a b) =
        (toElem a ++ toElem b)
instance XmlContent PackageId where
    fromElem (CElem (Elem "packageId" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "packageId" (toAttrs as) [])]
instance XmlAttributes PackageId where
    fromAttrs as =
        PackageId
          { packageIdId = definiteA fromAttrToStr "packageId" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (packageIdId v)
        ]
instance XmlContent File where
    fromElem (CElem (Elem "file" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (File a b), rest))
           (fromElem ca))
        (definite fromElem "<fileLocation>" "file" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (File a b) =
        [CElem (Elem "file" [] (toElem a ++ maybe [] toElem b))]
instance XmlContent FileLocation where
    fromElem (CElem (Elem "fileLocation" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (FileLocation a b), rest))
           (definite fromElem "<objectType>" "fileLocation" ca))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (FileLocation a b) =
        [CElem (Elem "fileLocation" [] (maybe [] toElem a ++ toElem b))]
instance XmlContent Files where
    fromElem (CElem (Elem "files" [] c0):rest) =
        (\(a,ca)->
           (Just (Files a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Files a) =
        [CElem (Elem "files" [] (concatMap toElem a))]
instance XmlContent FileVariants where
    fromElem (CElem (Elem "fileVariants" [] c0):rest) =
        (\(a,ca)->
           (Just (FileVariants a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (FileVariants a) =
        [CElem (Elem "fileVariants" [] (concatMap toElem a))]
instance XmlContent FileVariant where
    fromElem (CElem (Elem "fileVariant" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (FileVariant a b), rest))
           (fromElem ca))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (FileVariant a b) =
        [CElem (Elem "fileVariant" [] (maybe [] toElem a ++
                                       maybe [] toElem b))]
instance XmlContent FileContents where
    fromElem (CElem (Elem "fileContents" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "fileContents" (toAttrs as) [])]
instance XmlAttributes FileContents where
    fromAttrs as =
        FileContents
          { fileContentsDataBlock = definiteA fromAttrToStr "fileContents" "dataBlock" as
          , fileContentsCharType = defaultA fromAttrToTyp FileContents_charType_unicode "charType" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "dataBlock" (fileContentsDataBlock v)
        , defaultToAttr toAttrFrTyp "charType" (fileContentsCharType v)
        ]
instance XmlAttrType FileContents_charType where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "byte" = Just FileContents_charType_byte
            translate "unicode" = Just FileContents_charType_unicode
            translate _ = Nothing
    toAttrFrTyp n FileContents_charType_byte = Just (n, str2attr "byte")
    toAttrFrTyp n FileContents_charType_unicode = Just (n, str2attr "unicode")
instance XmlContent VersionInfo where
    fromElem (CElem (Elem "versionInfo" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (VersionInfo (fromAttrs as) a b), rest))
           (definite fromElem "<serverInfo>" "versionInfo" ca))
        (definite fromElem "<userInfo>" "versionInfo" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (VersionInfo as a b) =
        [CElem (Elem "versionInfo" (toAttrs as) (toElem a ++ toElem b))]
instance XmlAttributes VersionInfo_Attrs where
    fromAttrs as =
        VersionInfo_Attrs
          { versionInfoIsPresent = defaultA fromAttrToTyp VersionInfo_isPresent_present "isPresent" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "isPresent" (versionInfoIsPresent v)
        ]
instance XmlAttrType VersionInfo_isPresent where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "present" = Just VersionInfo_isPresent_present
            translate "absent" = Just VersionInfo_isPresent_absent
            translate _ = Nothing
    toAttrFrTyp n VersionInfo_isPresent_present = Just (n, str2attr "present")
    toAttrFrTyp n VersionInfo_isPresent_absent = Just (n, str2attr "absent")
instance XmlContent UserInfo where
    fromElem (CElem (Elem "userInfo" as c0):rest) =
        (\(a,ca)->
           (Just (UserInfo (fromAttrs as) a), rest))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (UserInfo as a) =
        [CElem (Elem "userInfo" (toAttrs as) (maybe [] toElem a))]
instance XmlAttributes UserInfo_Attrs where
    fromAttrs as =
        UserInfo_Attrs
          { userInfoLabel = possibleA fromAttrToStr "label" as
          , userInfoContents = possibleA fromAttrToStr "contents" as
          , userInfoVersion = possibleA fromAttrToStr "version" as
          , userInfoParents = possibleA fromAttrToStr "parents" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "label" (userInfoLabel v)
        , maybeToAttr toAttrFrStr "contents" (userInfoContents v)
        , maybeToAttr toAttrFrStr "version" (userInfoVersion v)
        , maybeToAttr toAttrFrStr "parents" (userInfoParents v)
        ]
instance XmlContent ServerInfo where
    fromElem (CElem (Elem "serverInfo" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "serverInfo" (toAttrs as) [])]
instance XmlAttributes ServerInfo where
    fromAttrs as =
        ServerInfo
          { serverInfoServerId = definiteA fromAttrToStr "serverInfo" "serverId" as
          , serverInfoSerialNo = definiteA fromAttrToStr "serverInfo" "serialNo" as
          , serverInfoTimeStamp = definiteA fromAttrToStr "serverInfo" "timeStamp" as
          , serverInfoUserId = definiteA fromAttrToStr "serverInfo" "userId" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "serverId" (serverInfoServerId v)
        , toAttrFrStr "serialNo" (serverInfoSerialNo v)
        , toAttrFrStr "timeStamp" (serverInfoTimeStamp v)
        , toAttrFrStr "userId" (serverInfoUserId v)
        ]
instance XmlContent ObjectName where
    fromElem (CElem (Elem "objectName" [] c0):rest) =
        (\(a,ca)->
           (Just (ObjectName a), rest))
        (definite fromText "text" "objectName" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ObjectName a) =
        [CElem (Elem "objectName" [] (toText a))]
instance XmlContent ObjectType where
    fromElem (CElem (Elem "objectType" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "objectType" (toAttrs as) [])]
instance XmlAttributes ObjectType where
    fromAttrs as =
        ObjectType
          { objectTypeBaseType = definiteA fromAttrToTyp "objectType" "baseType" as
          , objectTypeExtType = possibleA fromAttrToStr "extType" as
          , objectTypeExtraType = possibleA fromAttrToStr "extraType" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "baseType" (objectTypeBaseType v)
        , maybeToAttr toAttrFrStr "extType" (objectTypeExtType v)
        , maybeToAttr toAttrFrStr "extraType" (objectTypeExtraType v)
        ]
instance XmlAttrType ObjectType_baseType where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "folder" = Just ObjectType_baseType_folder
            translate "plainFile" = Just ObjectType_baseType_plainFile
            translate "mmissFolder" = Just ObjectType_baseType_mmissFolder
            translate "mmissObject" = Just ObjectType_baseType_mmissObject
            translate "mmissFile" = Just ObjectType_baseType_mmissFile
            translate "mmissPreamble" = Just ObjectType_baseType_mmissPreamble
            translate "unknownType" = Just ObjectType_baseType_unknownType
            translate _ = Nothing
    toAttrFrTyp n ObjectType_baseType_folder = Just (n, str2attr "folder")
    toAttrFrTyp n ObjectType_baseType_plainFile = Just (n, str2attr "plainFile")
    toAttrFrTyp n ObjectType_baseType_mmissFolder = Just (n, str2attr "mmissFolder")
    toAttrFrTyp n ObjectType_baseType_mmissObject = Just (n, str2attr "mmissObject")
    toAttrFrTyp n ObjectType_baseType_mmissFile = Just (n, str2attr "mmissFile")
    toAttrFrTyp n ObjectType_baseType_mmissPreamble = Just (n, str2attr "mmissPreamble")
    toAttrFrTyp n ObjectType_baseType_unknownType = Just (n, str2attr "unknownType")
instance XmlContent ObjectFullName where
    fromElem (CElem (Elem "objectFullName" [] c0):rest) =
        (\(a,ca)->
           (Just (ObjectFullName a), rest))
        (definite fromText "text" "objectFullName" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ObjectFullName a) =
        [CElem (Elem "objectFullName" [] (toText a))]
instance XmlContent WhichPermissions where
    fromElem (CElem (Elem "whichPermissions" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (WhichPermissionsServerRef a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (WhichPermissionsVersionRef_ObjectFullName a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (WhichPermissionsServerRef a) = [CElem (Elem "whichPermissions" [] (toElem a) )]
    toElem (WhichPermissionsVersionRef_ObjectFullName a) = [CElem (Elem "whichPermissions" [] (toElem a) )]
instance XmlContent Permissions where
    fromElem (CElem (Elem "permissions" [] c0):rest) =
        (\(a,ca)->
           (Just (Permissions a), rest))
        (definite fromText "text" "permissions" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Permissions a) =
        [CElem (Elem "permissions" [] (toText a))]
instance XmlContent Variant where
    fromElem (CElem (Elem "variant" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "variant" (toAttrs as) [])]
instance XmlAttributes Variant where
    fromAttrs as =
        Variant
          { variantKey = definiteA fromAttrToStr "variant" "key" as
          , variantValue = possibleA fromAttrToStr "value" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "key" (variantKey v)
        , maybeToAttr toAttrFrStr "value" (variantValue v)
        ]
instance XmlContent Variants where
    fromElem (CElem (Elem "variants" [] c0):rest) =
        (\(a,ca)->
           (Just (Variants a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Variants a) =
        [CElem (Elem "variants" [] (concatMap toElem a))]
instance XmlContent Attribute where
    fromElem (CElem (Elem "attribute" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "attribute" (toAttrs as) [])]
instance XmlAttributes Attribute where
    fromAttrs as =
        Attribute
          { attributeKey = definiteA fromAttrToStr "attribute" "key" as
          , attributeValue = definiteA fromAttrToStr "attribute" "value" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "key" (attributeKey v)
        , toAttrFrStr "value" (attributeValue v)
        ]
instance XmlContent Attributes where
    fromElem (CElem (Elem "attributes" [] c0):rest) =
        (\(a,ca)->
           (Just (Attributes a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Attributes a) =
        [CElem (Elem "attributes" [] (concatMap toElem a))]
instance XmlContent Messages where
    fromElem (CElem (Elem "messages" as c0):rest) =
        (\(a,ca)->
           (Just (Messages (fromAttrs as) a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Messages as a) =
        [CElem (Elem "messages" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Messages_Attrs where
    fromAttrs as =
        Messages_Attrs
          { messagesStatus = defaultA fromAttrToTyp Messages_status_success "status" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "status" (messagesStatus v)
        ]
instance XmlContent Messages_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (Messages_Alert a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (Messages_Error a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,rest) -> (Just (Messages_Warning a), rest)
                        (_,_) ->
                                case (fromElem c0) of
                                (Just a,rest) -> (Just (Messages_Message a), rest)
                                (_,_) ->
                                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Messages_Alert a) = toElem a
    toElem (Messages_Error a) = toElem a
    toElem (Messages_Warning a) = toElem a
    toElem (Messages_Message a) = toElem a
instance XmlAttrType Messages_status where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "success" = Just Messages_status_success
            translate "fail" = Just Messages_status_fail
            translate "panic" = Just Messages_status_panic
            translate _ = Nothing
    toAttrFrTyp n Messages_status_success = Just (n, str2attr "success")
    toAttrFrTyp n Messages_status_fail = Just (n, str2attr "fail")
    toAttrFrTyp n Messages_status_panic = Just (n, str2attr "panic")
instance XmlContent Alert where
    fromElem (CElem (Elem "alert" [] c0):rest) =
        (\(a,ca)->
           (Just (Alert a), rest))
        (definite fromText "text" "alert" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Alert a) =
        [CElem (Elem "alert" [] (toText a))]
instance XmlContent Error where
    fromElem (CElem (Elem "error" [] c0):rest) =
        (\(a,ca)->
           (Just (Error a), rest))
        (definite fromText "text" "error" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Error a) =
        [CElem (Elem "error" [] (toText a))]
instance XmlContent Warning where
    fromElem (CElem (Elem "warning" [] c0):rest) =
        (\(a,ca)->
           (Just (Warning a), rest))
        (definite fromText "text" "warning" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Warning a) =
        [CElem (Elem "warning" [] (toText a))]
instance XmlContent Message where
    fromElem (CElem (Elem "message" [] c0):rest) =
        (\(a,ca)->
           (Just (Message a), rest))
        (definite fromText "text" "message" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Message a) =
        [CElem (Elem "message" [] (toText a))]


{-Done-}
