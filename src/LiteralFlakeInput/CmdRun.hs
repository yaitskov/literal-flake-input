{-# LANGUAGE OverloadedRecordDot #-}
module LiteralFlakeInput.CmdRun where

import Data.Version (showVersion)
import Paths_literal_flake_input ( version )
import LiteralFlakeInput.Page ( Ypp(Ypp) )
import LiteralFlakeInput.CmdArgs ( CmdArgs(..) )
import LiteralFlakeInput.Prelude
    ( ($), Semigroup((<>)), IO, untag, putStrLn, trIo )
import Yesod.Core ( warp )

runCmd :: CmdArgs -> IO ()
runCmd = \case
  rs@(RunService {}) -> do
    $(trIo "start/rs")
    warp (untag rs.httpPortToListen) Ypp
  LiteralFlakeInputVersion ->
    putStrLn $ "Version " <> showVersion version
