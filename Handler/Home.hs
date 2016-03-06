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
    }
  deriving Show

getTag :: Maybe FilterOptions -> Maybe Text
getTag (Just (FilterOptions filterOptionsTag)) = Just filterOptionsTag
getTag Nothing = Nothing

filterForm :: Maybe FilterOptions -> AForm Handler FilterOptions
filterForm myFilterOptions = FilterOptions
    <$> areq textField (bfs ("Tag":: Text)) (getTag myFilterOptions)

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    case result of
         FormSuccess filterOptions' -> do
             allFiles <- runDB $ selectList [PhotoTag ==. filterOptionsTag filterOptions'] [Desc PhotoTimeShot]
             defaultLayout $(widgetFile "homepage")
         _ -> sendResponseStatus status404 ("Not Found"::Text)
