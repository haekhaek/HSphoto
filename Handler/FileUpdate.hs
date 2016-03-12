module Handler.FileUpdate where

import Import
import System.Directory

postFileUpdateR :: MediaFileId -> Handler Html
postFileUpdateR mediaFileId = do
    _ <- runDB $ get404 mediaFileId
    _ <- runDB $ update mediaFileId [MediaFileLikes +=. 1]
    sendResponseStatus status201 ("Updated"::Text)

deleteFileUpdateR :: MediaFileId -> Handler Html
deleteFileUpdateR mediaFileId = do
    fileToDelete <- runDB $ get404 mediaFileId
    case mediaFileContentType fileToDelete of
        "image/jpeg" -> deletePicture fileToDelete
        _ -> deleteVideo $ unpack $ mediaFileAbsolutePath fileToDelete
    runDB $ delete mediaFileId
    sendResponseStatus status201 ("Deleted"::Text)

deletePicture :: MediaFile -> HandlerT App IO ()
deletePicture fileToDelete = do
    let originalPicture = unpack $ mediaFileAbsolutePath fileToDelete
        thumbnail1 = (unpack $ mediaFileFolderPath fileToDelete) </> "w1-" ++ (unpack $ mediaFileName fileToDelete)
        thumbnail2 = (unpack $ mediaFileFolderPath fileToDelete) </> "w2-" ++ (unpack $ mediaFileName fileToDelete)
    liftIO $ removeFile $ originalPicture
    liftIO $ removeFile $ thumbnail1
    liftIO $ removeFile $ thumbnail2

deleteVideo :: String -> HandlerT App IO ()
deleteVideo filePath = do
    liftIO $ removeFile filePath
