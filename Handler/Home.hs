module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Data.Time.LocalTime
import Data.Time.Clock as Clock

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    let allFiles = []
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

data FilterOptions = FilterOptions
    { filterOptionsTag :: Maybe Text
    , cameraManufacturer :: Maybe Text
    , cameraModel :: Maybe Text
    , flashFired :: Maybe Bool
    , fromTimeShot :: Maybe Day
    , toTimeShot :: Maybe Day
    }
  deriving Show

filterForm :: Maybe FilterOptions -> AForm Handler FilterOptions
filterForm myFilterOptions = FilterOptions
    <$> aopt (jqueryAutocompleteField HomeR) tagSettings Nothing
    <*> aopt textField (bfs ("camera make"::Text)) Nothing
    <*> aopt textField (bfs ("camera model"::Text)) Nothing
    <*> aopt boolField flashFiredSettings Nothing
    <*> aopt datePickerField fromDateSettings Nothing
    <*> aopt datePickerField toDateSettings Nothing
    <*  bootstrapSubmit ("search" :: BootstrapSubmit Text)
    where fromDateSettings = (bfs ("from date photo created"::Text))
          toDateSettings = withSmallInput $ (bfs ("to date photo created"::Text))
          tagSettings = withPlaceholder "type in the tag you search for" $ tagSettings'
          tagSettings' = FieldSettings (SomeMessage ("tag"::Text)) Nothing (Just "tagId") Nothing []
          flashFiredSettings = (FieldSettings (SomeMessage ("flash fired"::Text)) Nothing (Just "flashFired") Nothing [])
          datePickerField = (jqueryDatePickerDayField def
            { jdsChangeYear=True
            , jdsYearRange="1950:−20"
            })

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (filterForm Nothing)
    case result of
         FormSuccess filterOptions' -> do
                let myFilter = map tupleToFilter . filter isFalseFilter $ createSqlFilters filterOptions'
                allFiles <- runDB $ selectList myFilter [Asc PhotoTimeShot]
                defaultLayout $(widgetFile "homepage")
         _ -> sendResponseStatus status404 ("Not Found"::Text)

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
isFalseFilter (True, a) = True
isFalseFilter (False, a) = False

checkIsBool :: Maybe a -> Bool
checkIsBool (Just a) = True
checkIsBool Nothing = False

createSqlFilters :: FilterOptions -> [(Bool, Filter Photo)]
createSqlFilters filterOptions =
    [(isJust $ filterOptionsTag filterOptions, Filter
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
        , filterValue=Right [dayToUtcTime diffTimeStartOfDay $ fromTimeShot filterOptions]
        , filterFilter=Gt
        }
    ),(isJust $ toTimeShot filterOptions, Filter
        { filterField=PhotoTimeShot
        , filterValue=Right [dayToUtcTime diffTimeEndOfDay $ toTimeShot filterOptions]
        , filterFilter=Lt
        }
    )]
