{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedLabels      #-}

module Main (main) where

import Prelude hiding (log)

import Polysemy (Member, Sem, runM)

import Colog (FieldType, LogAction, Message, Msg (..), RichMessage, RichMsg (..), Severity (Debug, Info), cmapM, defaultFieldMap, extractField, logByteStringStdout, richMessageAction, showSeverity, upgradeMessageAction)
import Colog.Message (FieldMap, showSourceLoc)
import Colog.Polysemy (Log, log, runLogAction)
import Control.Concurrent (ThreadId)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time.LocalTime (TimeZone (timeZoneMinutes), getCurrentTimeZone)
import GHC.Exts (IsList (fromList), toList)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import qualified Chronos as C
import qualified Chronos.Locale.English as C
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.TypeRepMap as TM
import qualified Data.Vector as Vector

log' :: HasCallStack => Member (Log Message) r => Severity -> Text -> Sem r ()
log' msgSeverity msgText =
    withFrozenCallStack (log Msg { msgStack = callStack, .. })

example :: Member (Log Message) r => Sem r ()
example = do
    log' Debug "First message..."
    log' Info  "Second message..."

-- Needed additional code -->
type instance FieldType "localTz"   = TimeZone

defaultFieldMapTz :: MonadIO m => FieldMap m
defaultFieldMapTz = fromList $ #localTz (liftIO getCurrentTimeZone) : toList defaultFieldMap

richMessageActionTz :: MonadIO m => LogAction m Message
richMessageActionTz = upgradeMessageAction defaultFieldMapTz $
    cmapM (fmap encodeUtf8 . fmtRichMessageDefaultTz) logByteStringStdout

fmtRichMessageDefaultTz :: MonadIO m => RichMessage m -> m Text
fmtRichMessageDefaultTz msg = fmtRichMessageCustomDefaultTz msg formatRichMessage
  where
    formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> Maybe TimeZone -> Message -> Text
    formatRichMessage (maybe "" showThreadId -> thread) maybeTime maybeTz Msg{..} =
        showSeverity msgSeverity
     <> time
     <> showSourceLoc msgStack
     <> thread
     <> msgText
     where time = maybe "" (showTime . C.timeToOffsetDatetime offset) maybeTime
           offset = C.Offset $ maybe 0 timeZoneMinutes maybeTz

fmtRichMessageCustomDefaultTz
    :: MonadIO m
    => RichMsg m msg
    -> (Maybe ThreadId -> Maybe C.Time -> Maybe TimeZone -> msg -> Text)
    -> m Text
fmtRichMessageCustomDefaultTz RichMsg{..} formatter = do
    maybeThreadId  <- extractField $ TM.lookup @"threadId"  richMsgMap
    maybePosixTime <- extractField $ TM.lookup @"posixTime" richMsgMap
    maybeLocalTz   <- extractField $ TM.lookup @"localTz"   richMsgMap
    pure $ formatter maybeThreadId maybePosixTime maybeLocalTz richMsgMsg

showTime :: C.OffsetDatetime -> Text
showTime t =
    square
    $ toStrict
    $ TB.toLazyText
    $ builderDmyHMSz t

builderDmyHMSz :: C.OffsetDatetime -> TB.Builder
builderDmyHMSz (C.OffsetDatetime (C.Datetime date time) offset) =
       builderDmy date
    <> spaceSep
    <> C.builder_HMS (C.SubsecondPrecisionFixed 3) (Just ':') time
    <> spaceSep
    <> C.builderOffset C.OffsetFormatColonOn offset
  where
    spaceSep :: TB.Builder
    spaceSep = TB.singleton ' '

    {- | Given a 'Date' construct a 'Text' 'TB.Builder'
    corresponding to a Day\/Month\/Year encoding.

    Example: @01 Jan 2020@
    -}
    builderDmy :: C.Date -> TB.Builder
    builderDmy (C.Date (C.Year y) m d) =
           zeroPadDayOfMonth d
        <> spaceSep
        <> TB.fromText (C.caseMonth C.abbreviated m)
        <> spaceSep
        <> TB.decimal y


    zeroPadDayOfMonth :: C.DayOfMonth -> TB.Builder
    zeroPadDayOfMonth (C.DayOfMonth d) =
        if d < 100
        then Vector.unsafeIndex twoDigitTextBuilder d
        else TB.decimal d

    twoDigitTextBuilder :: Vector.Vector TB.Builder
    twoDigitTextBuilder = Vector.fromList $
        map (TB.fromText . pack) twoDigitStrings
    {-# NOINLINE twoDigitTextBuilder #-}

    twoDigitStrings :: [String]
    twoDigitStrings =
        [ "00","01","02","03","04","05","06","07","08","09"
        , "10","11","12","13","14","15","16","17","18","19"
        , "20","21","22","23","24","25","26","27","28","29"
        , "30","31","32","33","34","35","36","37","38","39"
        , "40","41","42","43","44","45","46","47","48","49"
        , "50","51","52","53","54","55","56","57","58","59"
        , "60","61","62","63","64","65","66","67","68","69"
        , "70","71","72","73","74","75","76","77","78","79"
        , "80","81","82","83","84","85","86","87","88","89"
        , "90","91","92","93","94","95","96","97","98","99"
        ]

-- copied without change - maybe make public
showThreadId :: ThreadId -> Text
showThreadId = square . pack . show

square :: Text -> Text
square t = "[" <> t <> "] "

main :: IO ()
main = do
    runM $ runLogAction @IO richMessageAction example
    putStrLn "with tz..."
    runM $ runLogAction @IO richMessageActionTz example
