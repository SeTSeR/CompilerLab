module Lib
    ( compile
    , Config
    ) where

import Control.Monad.Except

compile :: Config -> ExceptT String IO ()
compile _ = lift $ putStrLn "someFunc"

data Config = Config { inputfile :: String
                     , outfile :: String
                     }