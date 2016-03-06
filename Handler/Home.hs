module Handler.Home where

import Import
import Text.Julius (RawJS (..))
import Yesod.Form.Bootstrap3

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    allFiles <- runDB $ selectList [] [Desc PhotoTime]
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
