module Handler.Tag where

import Import

-- data TagTerm = TagTerm { name::Text }
--
-- tagForm :: Maybe TagTerm -> AForm Handler TagTerm
-- tagForm foo = TagTerm
--     <$> areq textField "" Nothing

data TagTerm = TagTerm
    { value  :: Text
    }

instance ToJSON TagTerm where
    toJSON TagTerm {..} = object
        [ "value"  .= value
        ]

getTagR :: Handler TypedContent
getTagR = do
    maybeTagTerm <- lookupGetParam "term"
    selectRep $ do
        provideJson tagTerm
    where
        tagTerm@TagTerm {..} = TagTerm ((Just maybeTagTerm) ++ "Binens")
--     liftIO $ print maybeTagTerm
--     case maybeTagTerm of
--          Just(foo) -> sendResponseStatus status200 (bar foo)
--          Nothing -> sendResponseStatus status404 ("Not Found"::Text)
--
-- bar :: Text -> Text
-- bar foo = ("[{label:'Binens', value:'Binens'}]"::Text)
