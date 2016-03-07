module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Data.Time.LocalTime
import Data.Time.Clock as Clock

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
    , fromTimeShot :: Maybe Day
    , toTimeShot :: Maybe Day
    }
  deriving Show

getTag :: Maybe FilterOptions -> Maybe Text
getTag (Just (FilterOptions filterOptionsTag _ _ _ _ _)) = Just filterOptionsTag
getTag Nothing = Nothing

filterForm :: Maybe FilterOptions -> AForm Handler FilterOptions
filterForm myFilterOptions = FilterOptions
    <$> areq textField (bfs ("Tag":: Text)) (getTag myFilterOptions)
    <*> aopt textField (bfs ("Make"::Text)) Nothing
    <*> aopt textField (bfs ("Camramodel"::Text)) Nothing
    <*> aopt boolField "Flash fired" Nothing
    <*> aopt (jqueryDatePickerDayField def
        { jdsChangeYear=True
        , jdsYearRange="1950:−20"
        }) "From date photo created" Nothing
    <*> aopt (jqueryDatePickerDayField def
        { jdsChangeYear=True
        , jdsYearRange="1950:−20"
        }) "To date photo created" Nothing

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    case result of
         FormSuccess filterOptions' -> do
                let myFilter = map tupleToFilter . filter isFalseFilter $ createSqlFilters filterOptions'
                allFiles <- runDB $ selectList myFilter [Desc PhotoTimeShot]
                defaultLayout $(widgetFile "homepage")
         _ -> sendResponseStatus status404 ("Not Found"::Text)

dayToUtcTime :: Maybe Day -> Maybe UTCTime
dayToUtcTime (Just myDay) = Just $ UTCTime myDay $ Clock.secondsToDiffTime 0
dayToUtcTime Nothing = Nothing

tupleToFilter :: (Bool, a) -> a
tupleToFilter (_, a) = a

isFalseFilter :: (Bool, a) -> Bool
isFalseFilter (True, a) = True
isFalseFilter (False, a) = False

checkIsBool :: Maybe a -> Bool
checkIsBool (Just a) = True
checkIsBool Nothing = False

createSqlFilters :: FilterOptions -> [(Bool, Filter Photo)]
createSqlFilters filterOptions =
    [(True, Filter
        { filterField=PhotoTag
        , filterValue=Right [filterOptionsTag filterOptions]
        , filterFilter=Eq
        }
     ),(isJust $ cameraManufacturer filterOptions, Filter
        { filterField=PhotoCameraManufacturer
        , filterValue=Right [cameraManufacturer filterOptions]
        , filterFilter=Eq
        }
     ),(isJust $ cameraModel filterOptions, Filter
        { filterField=PhotoCameraModel
        , filterValue=Right [cameraModel filterOptions]
        , filterFilter=Eq
        }
    ),(isJust $ flashFired filterOptions, Filter
        { filterField=PhotoFlashFired
        , filterValue=Right [flashFired filterOptions]
        , filterFilter=Eq
        }
    ),(isJust $ fromTimeShot filterOptions, Filter
        { filterField=PhotoTimeShot
        , filterValue=Right [dayToUtcTime $ fromTimeShot filterOptions]
        , filterFilter=Gt
        }
    ),(isJust $ toTimeShot filterOptions, Filter
        { filterField=PhotoTimeShot
        , filterValue=Right [dayToUtcTime $ toTimeShot filterOptions]
        , filterFilter=Lt
        }
    )]
