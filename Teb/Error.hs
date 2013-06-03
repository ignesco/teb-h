module Teb.Error where

import Teb.Types

reportError :: Arguments -> IO ()
reportError args = putStrLn $ "Incorrect parameters for teb command: " ++ head args
