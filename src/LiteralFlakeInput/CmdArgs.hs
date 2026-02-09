module LiteralFlakeInput.CmdArgs where

import Options.Applicative
import LiteralFlakeInput.Prelude


data HttpPort
data Cert
data CertKey

data CmdArgs
  = RunService
    { httpPortToListen :: Tagged HttpPort Int
    , certFile :: Tagged Cert FilePath
    , keyFile :: Tagged CertKey FilePath
    }
  | LiteralFlakeInputVersion
    deriving (Eq, Show)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    serviceP = RunService <$> portOption <*> certO <*> certKeyO
    cmdp =
      hsubparser
        (  command "run" (infoP serviceP "launch the service exposed over HTTP")
        <> command "version" (infoP (pure LiteralFlakeInputVersion) "print program version"))

    infoP p h = info p (progDesc h <> fullDesc)
    phelp =
      progDesc
        "Translates HTTP GET params into Nix attrset"

portOption :: Parser (Tagged HttpPort Int)
portOption = Tagged <$>
  option auto
  ( long "port"
    <> short 'p'
    <> showDefault
    <> value 3042
    <> help "HTTP port to listen"
    <> metavar "PORT"
  )

certO :: Parser (Tagged Cert FilePath)
certO = Tagged <$>
  strOption
  ( long "certificate"
    <> short 'c'
    <> showDefault
    <> value "certificate.pem"
    <> help "path to SSL certificate file"
    <> metavar "CERT"
  )
certKeyO :: Parser (Tagged CertKey FilePath)
certKeyO = Tagged <$>
  strOption
  ( long "key"
    <> short 'k'
    <> showDefault
    <> value "key.pem"
    <> help "path to key file of SSL certificate"
    <> metavar "KEY"
  )
