module Handler.File where

import Import

getFileR :: Handler Html
getFileR = defaultLayout $ do
    setTitle("Upload files")
    $(widgetFile "files/index")
