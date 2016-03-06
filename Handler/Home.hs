module Handler.Home where

import Import
import Yesod.Form.Bootstrap3

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    allFiles <- runDB $ selectList [] [Desc PhotoTime]
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

data FilterOptions = FilterOptions
    { filterOptionsTag :: Text
    , cameraManufacturer :: Maybe Text
    , cameraModel :: Maybe Text
    , flashFired :: Maybe Bool
    }
  deriving Show

getTag :: Maybe FilterOptions -> Maybe Text
getTag (Just (FilterOptions filterOptionsTag _ _ _)) = Just filterOptionsTag
getTag Nothing = Nothing

filterForm :: Maybe FilterOptions -> AForm Handler FilterOptions
filterForm myFilterOptions = FilterOptions
    <$> areq textField (bfs ("Tag":: Text)) (getTag myFilterOptions)
    <*> aopt textField (bfs ("Make"::Text)) Nothing
    <*> aopt textField (bfs ("Camramodel"::Text)) Nothing
    <*> aopt checkBoxField "Flash fired" Nothing

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    case result of
         FormSuccess filterOptions' -> do
             allFiles <- runDB $ selectList
                (       [PhotoTag ==. filterOptionsTag filterOptions']
                    ||. [PhotoCameraManufacturer ==. cameraManufacturer filterOptions']
                    ||. [PhotoCameraModel ==. cameraModel filterOptions']
                    ||. [PhotoFlashFired ==. flashFired filterOptions']
                )
                 [Desc PhotoTimeShot]
             defaultLayout $(widgetFile "homepage")
         _ -> sendResponseStatus status404 ("Not Found"::Text)
