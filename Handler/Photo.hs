module Handler.Photo where

import Import
import System.Directory
import Yesod.Form.Bootstrap3
import Data.UUID
import System.Random
import Graphics.HsExif as HsExif
import Data.Time.LocalTime
import Graphics.ThumbnailPlus as TP
import qualified System.Directory as D
import Control.Concurrent (forkIO)
import qualified Control.Monad.Trans.Resource as R

postPhotoR :: Handler Html
postPhotoR = do
    ((result, _), _) <- runFormPost fileForm
    case result of
        FormSuccess myForm -> do
            saveTagName $ snd' myForm

            uploadedPhotoWithDefaults <- formToFile myForm
            uploadedPhoto <- updateExifMap uploadedPhotoWithDefaults

            let photoPath = mediaFileFolderPath uploadedPhoto

            _ <- liftIO $ forkIO $ R.runResourceT $ do
                myThumbnails <- TP.createThumbnails thumbnailConfig $ unpack $ mediaFileAbsolutePath uploadedPhoto
                case myThumbnails of
                    TP.CreatedThumbnails thumbnails _ -> liftIO $ copyThumbnails thumbnails photoPath (mediaFileName uploadedPhoto)
                    TP.ImageSizeTooLarge _ -> liftIO $ print ("ImageSizeTooLarge"::Text)
                    TP.FileSizeTooLarge _ -> liftIO $ print ("FileSizeTooLarge"::Text)
                    TP.ImageFormatUnrecognized -> liftIO $ print ("ImageFormatUnrecognized"::Text)

            _ <- runDB $ insert uploadedPhoto
            sendResponseStatus status204 ("No Content"::Text)
        _ -> sendResponseStatus status400 ("Bad Request"::Text)

snd' :: (FileInfo, Text, UTCTime, Text) -> Text
snd' (_, tagName, _, _) = tagName

saveTagName :: Text -> Handler ()
saveTagName tagValue = do
    maybeTagId <- runDB $ selectFirst [TagValue ==. tagValue] []
    case maybeTagId of
        Nothing -> do
            _ <- runDB $ insert $ Tag tagValue
            return ()
        Just _ -> return ()

thumbnailConfig :: TP.Configuration
thumbnailConfig = TP.Configuration (5 * 1024 * 1024) (Size 6144 6144) SameFileFormat [(Size 600 600, Nothing), (Size 1360 1360, Nothing)] D.getTemporaryDirectory

copyThumbnails :: [Thumbnail] -> Text -> String -> IO ()
copyThumbnails (th1:th2:_) path originalFileName = do
    copyFile (TP.thumbFp th1) (unpack path </> "w1-" ++ originalFileName)
    copyFile (TP.thumbFp th2) (unpack path </> "w2-" ++ originalFileName)
copyThumbnails _ path originalFileName = do
    copyFile (unpack path </> originalFileName) (unpack path </> "w1-" ++ originalFileName)
    copyFile (unpack path </> originalFileName) (unpack path </> "w2-" ++ originalFileName)

fileForm :: Form (FileInfo, Text, UTCTime, Text)
fileForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
    <$> fileAFormReq "Add file"
    <*> areq textField (bfs ("Tag" :: Text)) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> pure "/tmp"

formToFile :: (FileInfo, Text, UTCTime, Text) -> Handler MediaFile
formToFile (file, tag, time, _) = do
    folderPath <- createFolder time
    fileName <- moveToUploadFolder file folderPath
    return MediaFile {
        mediaFileName=fileName,
        mediaFileTag=tag,
        mediaFileTime=time,
        mediaFileContentType=fileContentType file,
        mediaFileFolderPath=pack folderPath,
        mediaFileAbsolutePath=pack $ folderPath </> fileName,
        mediaFileGpsLat=Just ("0"::Text),
        mediaFileGpsLong=Just ("0"::Text),
        mediaFileCameraModel=Just ("None"::Text),
        mediaFileCameraManufacturer=Just ("None"::Text),
        mediaFileFlashFired= Just False,
        mediaFileTimeShot=Just time,
        mediaFileLikes=0
    }

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

updateExifMap :: MediaFile -> Handler MediaFile
updateExifMap uploadedPhoto = do
    eMap <- liftIO $ HsExif.parseFileExif $ unpack $ mediaFileAbsolutePath uploadedPhoto
    timeZone <- liftIO $ getCurrentTimeZone
    case eMap of
         Left _ -> return uploadedPhoto
         Right val -> return $ updateExifData uploadedPhoto val timeZone

updateExifData :: MediaFile -> (Map ExifTag ExifValue) -> TimeZone -> MediaFile
updateExifData uploadedPhoto exifMap timeZone = uploadedPhoto {
        mediaFileCameraModel=lookupTag HsExif.model exifMap,
        mediaFileCameraManufacturer=lookupTag HsExif.make exifMap,
        mediaFileFlashFired=HsExif.wasFlashFired exifMap,
        mediaFileTimeShot=(localTimeToUTC timeZone <$> HsExif.getDateTimeOriginal exifMap)
    }

lookupTag :: ExifTag -> (Map ExifTag ExifValue) -> Maybe Text
lookupTag tag exifMap = packTagValue $ lookup tag exifMap

packTagValue :: Maybe (MapValue (Map ExifTag ExifValue)) -> Maybe Text
packTagValue (Just tagValue) = Just (pack $ show $ tagValue)
packTagValue Nothing = Nothing
