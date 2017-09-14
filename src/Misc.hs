module Misc where

import System.IO (hFlush, stdout, getLine)

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine