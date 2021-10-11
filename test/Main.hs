{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}

import Test.Tasty (defaultMain,testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Show.Pretty (ppShow)

import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Bytes as Bytes
import qualified Panos.Api.Xml.Licenses as Licenses
import qualified Xml

main :: IO ()
main = defaultMain $ testGroup "sample"
  [ goldenVsString "license" "sample/license/response.txt" $ do
      doc <- loadXml "sample/license/response.xml"
      case doc of
        Xml.Element Xml.Content{Xml.tag="response",Xml.children=[Xml.Element Xml.Content{Xml.tag="result",Xml.children=[e]}]} -> do
          licenses <- maybe (fail "Could not parse licenses") pure (Licenses.parse e)
          pure (LBC8.pack (ppShow licenses) <> "\n")
        _ -> fail "Response did not have expected wrapper"
  ]

loadXml :: String -> IO Xml.Node
loadXml filename = do
  contents <- Bytes.readFile filename
  case Xml.decode contents of
    Nothing -> fail ("Could not parse document: " ++ filename)
    Just r -> pure r
