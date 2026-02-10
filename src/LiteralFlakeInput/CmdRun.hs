{-# LANGUAGE OverloadedRecordDot #-}
module LiteralFlakeInput.CmdRun where

import Data.Version (showVersion)
import Paths_literal_flake_input ( version )
import LiteralFlakeInput.Page ( Ypp(Ypp) )
import LiteralFlakeInput.CmdArgs

import LiteralFlakeInput.Prelude
import Yesod.Core
import Yesod.Core.Types ( Logger )
import Network.Wai.Handler.WarpTLS ( runTLS, tlsSettings, TLSSettings )
import Network.Wai.Handler.Warp
    ( Settings,
      setOnException,
      setPort,
      setServerName,
      defaultSettings,
      defaultShouldDisplayException )

import Language.Haskell.TH.Syntax (qLocation)
import Control.Monad.Logger
    (  liftLoc, ToLogStr(toLogStr) )


mkSettings :: CmdArgs -> Logger -> Settings
mkSettings ca logger =
  setPort (untag ca.httpPortToListen) $
  setServerName "LiteralFlakeInput" $
  setOnException onEx
  defaultSettings
  where
    shouldLog' = defaultShouldDisplayException
    onEx _ e =
      when (shouldLog' e) $
      messageLoggerSource
      Ypp
      logger
      $(qLocation >>= liftLoc)
      "yesod-core"
      LevelError
      (toLogStr $ "Exception from Warp: " ++ show e)

mkTlsSettings :: Tagged Cert FilePath -> Tagged CertKey FilePath -> TLSSettings
mkTlsSettings cert key = tlsSettings (untag cert) (untag key)

runCmd :: CmdArgs -> IO ()
runCmd = \case
  rs@RunService {} -> do
    $(trIo "start/rs")
    let y = Ypp
    case liftA2 mkTlsSettings rs.certFile rs.keyFile of
      Nothing -> warp (untag rs.httpPortToListen) y
      Just tlsSngs -> do
        logger <- makeLogger y
        runTLS tlsSngs (mkSettings rs logger) =<< toWaiApp y
  LiteralFlakeInputVersion ->
    putStrLn $ "Version " <> showVersion version
