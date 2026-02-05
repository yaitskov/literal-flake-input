module Main where

import LiteralFlakeInput.CmdArgs
import LiteralFlakeInput.CmdRun
import LiteralFlakeInput.Prelude

main :: IO ()
main = execWithArgs runCmd
