module Handler.Video where

import Import
import Handler.Photo

postVideoR :: Handler Html
postVideoR = do
    ((result, _), _) <- runFormPost $ fileForm
    case result of
        FormSuccess myForm -> do
            uploadedVideo <- formToFile myForm
            videoId <- runDB $ insert uploadedVideo
            sendResponseStatus status204 ("No Content"::Text)
        _ -> sendResponseStatus status400 ("Bad Request"::Text)
