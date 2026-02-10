{-# LANGUAGE OverloadedRecordDot #-}
module LiteralFlakeInput.CmdRun where

import Data.Version (showVersion)
import Paths_literal_flake_input ( version )
import LiteralFlakeInput.Page ( Ypp(Ypp) )
import LiteralFlakeInput.CmdArgs ( CmdArgs(..), CertKey, Cert )

import LiteralFlakeInput.Prelude
import Yesod.Core
    ( toWaiApp,
      LogLevel(LevelError),
      Application,
      Yesod(makeLogger, messageLoggerSource) )
import Yesod.Core.Types ( Logger )
import Network.Wai.Handler.WarpTLS ( runTLS, tlsSettings, TLSSettings )
import Network.Wai.Handler.Warp
    ( Settings,
      setBeforeMainLoop,
      setMaxTotalHeaderLength,
      setOnException,
      setPort,
      setServerName,
      setSlowlorisSize,
      setTimeout,
      runSettings,
      defaultSettings,
      defaultShouldDisplayException )

import Language.Haskell.TH.Syntax (qLocation)
import Control.Monad.Logger
    (  liftLoc, ToLogStr(toLogStr) )


mkSettings :: CmdArgs -> Logger -> Settings
mkSettings ca logger =
  setPort port $
  setServerName "LiteralFlakeInput" $
  setOnException onEx $
  setSlowlorisSize 1024 $
  setMaxTotalHeaderLength 1024 $
  setBeforeMainLoop
  (putStrLn $ "Go http://localhost:" <> show port <> "/hello/42/prod/true/plugin/null/name/Alice") $
  setTimeout 9
  defaultSettings

  where
    port = untag ca.httpPortToListen
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

runPlain :: Settings -> Application -> IO ()
runPlain = runSettings

runCmd :: CmdArgs -> IO ()
runCmd = \case
  rs@RunService {} -> do
    $(trIo "start/rs")
    let y = Ypp
    logger <- makeLogger y
    case liftA2 mkTlsSettings rs.certFile rs.keyFile of
      Nothing -> runPlain (mkSettings rs logger) =<< toWaiApp y
      Just tlsSngs -> runTLS tlsSngs (mkSettings rs logger) =<< toWaiApp y
  LiteralFlakeInputVersion ->
    putStrLn $ "Version " <> showVersion version
