module Handler.FileUpdate where

import Import

postFileUpdateR :: MediaFileId -> Handler Html
postFileUpdateR mediaFileId = do
    _ <- runDB $ get404 mediaFileId
    newValue <- runDB $ update mediaFileId [MediaFileLikes +=. 1]
    sendResponseStatus status201 ("Updated"::Text)

deleteFileUpdateR :: MediaFileId -> Handler Html
deleteFileUpdateR mediaFileId = error "Not yet implemented: deleteFileUpdateR"
