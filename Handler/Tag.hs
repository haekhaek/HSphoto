module Handler.Tag where

import Import
import qualified Database.Esqueleto as P

getTagR :: Handler Value
getTagR = do
    maybeTagValue <- lookupGetParam "term"
    case maybeTagValue of
        Just(tagValue) -> do
            tags <- runDB $ P.select $ P.from $ \tag -> do
                P.where_ (tag P.^. TagValue  `P.like` (P.%) P.++. P.val tagValue P.++. (P.%))
                return tag
            returnJson tags
        Nothing -> sendResponseStatus status404 ("Not Found"::Text)
