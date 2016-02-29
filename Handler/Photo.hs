module Handler.Photo where

import Import
import System.Directory
import Yesod.Form.Bootstrap3
import Data.UUID
import System.Random

fileForm :: Form (FileInfo, Text, UTCTime, Text)
fileForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
    <$> fileAFormReq "Add file"
    <*> areq textField (bfs ("Tag" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> pure "/tmp"

postPhotoR :: Handler Html
postPhotoR = do
    ((result, _), _) <- runFormPost $ fileForm
    case result of
        FormSuccess (file, tag, time, _) -> do
            folderPath <- createFolder time
            fileName <- moveToUploadFolder file folderPath
            _ <- runDB $ insert $ Photo fileName tag time (fileContentType file) $ pack folderPath
            sendResponseStatus status204 ("No Content"::Text)
        _ -> sendResponseStatus status400 ("Bad Request"::Text)

createFolder :: UTCTime -> Handler FilePath
createFolder time = do
    master <- getYesod
    let baseUploadFolder = unpack $ appUploadFolder $ appSettings master
        folderPath = baseUploadFolder </> getDateFormated time </> getHourFormated time </> getMinuteFormated time
    liftIO $ createDirectoryIfMissing True folderPath
    return folderPath

getDateFormated :: UTCTime -> String
getDateFormated time = formatTime' time "%F"

getHourFormated :: UTCTime -> String
getHourFormated time = formatTime' time "%H"

getMinuteFormated :: UTCTime -> String
getMinuteFormated time = formatTime' time "%M"

formatTime' :: UTCTime -> String -> String
formatTime' time formatCharacter = formatTime defaultTimeLocale formatCharacter time

moveToUploadFolder :: FileInfo -> FilePath -> Handler FilePath
moveToUploadFolder file folderPath = do
    uniqueFileName <- createUniqueFileName file
    let uniqueFileName' = unpack $ uniqueFileName
        path = createFilePath folderPath $ uniqueFileName'
    liftIO $ fileMove file path
    return uniqueFileName'

createFilePath :: FilePath -> String -> FilePath
createFilePath folder fileName = folder </> fileName

-- ios devices upload every single image with the name "image.jpeg"
-- that's why we need uniqueFileName for every file
createUniqueFileName :: FileInfo -> Handler Text
createUniqueFileName file = do
    uniqueFileName <- liftIO randomIO
    return $ (toText uniqueFileName) ++ (fileName file)
