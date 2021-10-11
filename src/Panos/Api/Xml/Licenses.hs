{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Panos.Api.Xml.Licenses
  ( License(..)
  , parse
  ) where

import Data.Primitive (SmallArray)
import Panos.Api.Xml.License (License)

import qualified Panos.Api.Xml.License as License
import qualified Xml as X

parse :: X.Node -> Maybe (SmallArray License)
parse = \case
  X.Text{} -> Nothing
  X.Element X.Content{tag,children} -> if tag == "licenses"
    then traverse License.parse children
    else Nothing
