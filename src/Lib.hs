module Lib
    ( compile
    , config
    , Config
    ) where

import Control.Monad.Except

compile :: Config -> ExceptT String IO ()
compile _ = lift $ putStrLn "someFunc"

config :: String -> String -> Config
config infile outfile = Config { inputfile = infile
                               , outfile = outfile
                               }

data Config = Config { inputfile :: String
                     , outfile :: String
                     }