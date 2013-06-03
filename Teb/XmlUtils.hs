module Teb.XmlUtils where

import Data.Text (pack, unpack, breakOn)
import Text.XML.HXT.Core

dropHeader :: String -> String
dropHeader xml = ((drop 2).unpack.snd) $ breakOn (pack "?>") (pack xml)

runLAXml :: LA String b -> String -> [b]
runLAXml arrow string = runLA arrow ("<__LAXRoot>"++(dropHeader string)++"</__LAXRoot>")
