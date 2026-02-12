{-# LANGUAGE OverloadedRecordDot #-}
module LiteralFlakeInput.CmdRun where

import Control.Monad.Logger ( liftLoc, ToLogStr(toLogStr) )
import Data.Version (showVersion)
import Language.Haskell.TH.Syntax (qLocation)
import LiteralFlakeInput.Page ( Ypp(Ypp), YppMetrics (YppMetrics) )
import LiteralFlakeInput.CmdArgs ( CmdArgs(..), CertKey, Cert )
import LiteralFlakeInput.Prelude
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
import Paths_literal_flake_input ( version )
import System.Metrics ( createCounter, newStore, registerGcMetrics )
import System.Remote.Monitoring.Wai ( forkServerWith )
import Yesod.Core
    ( toWaiApp,
      LogLevel(LevelError),
      Application,
      Yesod(makeLogger, messageLoggerSource) )
import Yesod.Core.Types ( Logger )

mkSettings :: Ypp -> CmdArgs -> Logger -> Settings
mkSettings yp ca logger =
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
      yp
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
    st <- newStore
    registerGcMetrics st
    myMetrics <- YppMetrics <$> createCounter "landing.requests" st <*> createCounter "api.requests" st
    forM_ rs.ekgPort $ \(Tagged ep) -> do
      putStrLn $ "Launch EKG on port " <> show ep
      forkServerWith st "0.0.0.0" ep

    $(trIo "start/rs")
    let y = Ypp st myMetrics
    logger <- makeLogger y
    case liftA2 mkTlsSettings rs.certFile rs.keyFile of
      Nothing -> runPlain (mkSettings y rs logger) =<< toWaiApp y
      Just tlsSngs -> runTLS tlsSngs (mkSettings y rs logger) =<< toWaiApp y
  LiteralFlakeInputVersion ->
    putStrLn $ "Version " <> showVersion version
