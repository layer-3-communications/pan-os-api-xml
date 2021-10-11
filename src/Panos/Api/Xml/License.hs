{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Panos.Api.Xml.License
  ( License(..)
  , parse
  ) where

import Control.Monad (when)
import Data.Text.Short (ShortText)
import Data.Primitive (SmallArray)
import Chronos (Day,Date(Date))
import GHC.Exts (Ptr(Ptr))

import qualified Chronos
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Primitive as PM
import qualified Data.Text.Short as TS
import qualified Xml as X

data License = License
  { feature :: !ShortText
  , description :: !ShortText
  , serial :: !ShortText
  , issued :: !Day
  , expires :: !Day
  , expired :: !Bool
  } deriving stock (Show)

parse :: X.Node -> Maybe License
parse = \case
  X.Text{} -> Nothing
  X.Element X.Content{tag,children} -> if tag == "entry"
    then parseFields children
    else Nothing

parseFields :: SmallArray X.Node -> Maybe License
parseFields fields = do
  feature <- singletonTextNode "feature" fields
  description <- singletonTextNode "description" fields
  serial <- singletonTextNode "serial" fields
  issuedText <- singletonTextNode "issued" fields
  issuedDate <- P.parseBytesMaybe dateParser (Bytes.fromShortByteString (TS.toShortByteString issuedText))
  let !issued = Chronos.dateToDay issuedDate
  expiresText <- singletonTextNode "expires" fields
  expiresDate <- P.parseBytesMaybe dateParser (Bytes.fromShortByteString (TS.toShortByteString expiresText))
  let !expires = Chronos.dateToDay expiresDate
  expiredText <- singletonTextNode "expired" fields
  expired <- case expiredText of
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing
  pure License{feature,description,serial,issued,expires,expired}

singletonTextNode :: ShortText -> SmallArray X.Node -> Maybe ShortText
singletonTextNode !name fields =
  let r = foldr
        (\x acc -> case x of
          X.Text{} -> acc
          X.Element X.Content{tag,children} -> if tag == name
            then Just children
            else acc
        ) Nothing fields
   in case r of
        Nothing -> Nothing
        Just xs -> if PM.sizeofSmallArray xs == 1
          then case PM.indexSmallArray xs 0 of
            X.Text t -> Just t
            X.Element{} -> Nothing
          else Nothing

dateParser :: P.Parser () s Date
dateParser = do
  monthName <- Latin.takeTrailedBy () ' '
  month <-
    if | Bytes.equalsCString (Ptr "January"#) monthName -> pure Chronos.january
       | Bytes.equalsCString (Ptr "February"#) monthName -> pure Chronos.february
       | Bytes.equalsCString (Ptr "March"#) monthName -> pure Chronos.march
       | Bytes.equalsCString (Ptr "April"#) monthName -> pure Chronos.april
       | Bytes.equalsCString (Ptr "May"#) monthName -> pure Chronos.may
       | Bytes.equalsCString (Ptr "June"#) monthName -> pure Chronos.june
       | Bytes.equalsCString (Ptr "July"#) monthName -> pure Chronos.july
       | Bytes.equalsCString (Ptr "August"#) monthName -> pure Chronos.august
       | Bytes.equalsCString (Ptr "September"#) monthName -> pure Chronos.september
       | Bytes.equalsCString (Ptr "October"#) monthName -> pure Chronos.october
       | Bytes.equalsCString (Ptr "November"#) monthName -> pure Chronos.november
       | Bytes.equalsCString (Ptr "December"#) monthName -> pure Chronos.december
       | otherwise -> P.fail ()
  dayOfMonth <- Latin.decWord8 ()
  when (dayOfMonth == 0 || dayOfMonth >= 32) (P.fail ())
  Latin.char2 () ',' ' '
  year <- Latin.decWord16 ()
  when (year < 1970 || year >= 2200) (P.fail ())
  P.endOfInput ()
  pure (Date (Chronos.Year (fromIntegral year)) month (Chronos.DayOfMonth (fromIntegral dayOfMonth)))
  
