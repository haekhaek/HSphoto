module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Data.Time.Clock as Clock

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterForm
    let allFiles = []
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

filterForm :: AForm Handler FilterOptions
filterForm = FilterOptions
    <$> areq (jqueryAutocompleteField TagR) tagSettings Nothing
    <*> aopt textField (bfs ("camera make"::Text)) Nothing
    <*> aopt textField (bfs ("camera model"::Text)) Nothing
    <*> aopt boolField flashFiredSettings Nothing
    <*> aopt datePickerField fromDateSettings Nothing
    <*> aopt datePickerField toDateSettings Nothing
    <*  bootstrapSubmit ("search" :: BootstrapSubmit Text)
    where fromDateSettings = (bfs ("date start"::Text))
          toDateSettings = withSmallInput $ (bfs ("date end"::Text))
          tagSettings = withPlaceholder "type in the tag you search for" $ tagSettings'
          tagSettings' = FieldSettings (SomeMessage ("tag"::Text)) Nothing (Just "tagId") Nothing []
          flashFiredSettings = (FieldSettings (SomeMessage ("flash fired"::Text)) Nothing (Just "flashFired") Nothing [])
          datePickerField = (jqueryDatePickerDayField def
            { jdsChangeYear=True
            , jdsYearRange="1950:−20"
            })

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterForm
    case result of
         FormSuccess filterOptions' -> do
                let myFilter = map tupleToFilter . filter isFalseFilter $ createSqlFilters filterOptions'
                allFiles <- runDB $ selectList myFilter [Asc MediaFileTime]
                defaultLayout $(widgetFile "homepage")
         _ -> defaultLayout $ do
             let allFiles = []
             $(widgetFile "homepage")

dayToUtcTime :: DiffTime -> Maybe Day -> Maybe UTCTime
dayToUtcTime diff (Just myDay) = Just $ UTCTime myDay $ diff
dayToUtcTime _ Nothing = Nothing

diffTimeStartOfDay :: DiffTime
diffTimeStartOfDay = Clock.secondsToDiffTime 0

diffTimeEndOfDay :: DiffTime
diffTimeEndOfDay = Clock.secondsToDiffTime 86400

tupleToFilter :: (Bool, a) -> a
tupleToFilter (_, a) = a

isFalseFilter :: (Bool, a) -> Bool
isFalseFilter (True, _) = True
isFalseFilter (False, _) = False

checkIsBool :: Maybe a -> Bool
checkIsBool (Just _) = True
checkIsBool Nothing = False

createSqlFilters :: FilterOptions -> [(Bool, Filter MediaFile)]
createSqlFilters filterOptions =
    [(True, Filter
        { filterField=MediaFileTag
        , filterValue=Right [filterOptionsTag filterOptions]
        , filterFilter=Eq
        }
     ),(isJust $ cameraManufacturer filterOptions, Filter
        { filterField=MediaFileCameraManufacturer
        , filterValue=Right [cameraManufacturer filterOptions]
        , filterFilter=Eq
        }
     ),(isJust $ cameraModel filterOptions, Filter
        { filterField=MediaFileCameraModel
        , filterValue=Right [cameraModel filterOptions]
        , filterFilter=Eq
        }
    ),(isJust $ flashFired filterOptions, Filter
        { filterField=MediaFileFlashFired
        , filterValue=Right [flashFired filterOptions]
        , filterFilter=Eq
        }
    ),(isJust $ fromTimeShot filterOptions, Filter
        { filterField=MediaFileTimeShot
        , filterValue=Right [dayToUtcTime diffTimeStartOfDay $ fromTimeShot filterOptions]
        , filterFilter=Gt
        }
    ),(isJust $ toTimeShot filterOptions, Filter
        { filterField=MediaFileTimeShot
        , filterValue=Right [dayToUtcTime diffTimeEndOfDay $ toTimeShot filterOptions]
        , filterFilter=Lt
        }
    )]

isImageFile :: Text -> Bool
isImageFile "image/jpeg" = True
isImageFile "image/gif" = True
isImageFile "image/png" = True
isImageFile _ = False
