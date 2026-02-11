module LiteralFlakeInput.CmdArgs where

import Options.Applicative
import LiteralFlakeInput.Prelude


data HttpPort
data EkgPort
data Cert
data CertKey

data CmdArgs
  = RunService
    { httpPortToListen :: Tagged HttpPort Int
    , certFile :: Maybe (Tagged Cert FilePath)
    , keyFile :: Maybe (Tagged CertKey FilePath)
    , ekgPort :: Maybe (Tagged EkgPort Int)
    }
  | LiteralFlakeInputVersion
    deriving (Eq, Show)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    serviceP = RunService <$> portOption <*> certO <*> certKeyO <*> ekgPortOption
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

ekgPortOption :: Parser (Maybe (Tagged EkgPort Int))
ekgPortOption = pured . zeroToNothing <$>
  option auto
  ( long "ekg-port"
    <> short 'e'
    <> value 0
    <> help "HTTP port for EKG monitoring. Zero to disable."
    <> metavar "EKG"
  )

emptyToNothing :: FilePath -> Maybe FilePath
emptyToNothing "" = Nothing
emptyToNothing s = Just s

zeroToNothing :: Int -> Maybe Int
zeroToNothing s | s <= 0 = Nothing
                | otherwise = Just s

pured :: (Applicative g, Applicative f) => g a -> g (f a)
pured = fmap pure

certO :: Parser (Maybe (Tagged Cert FilePath))
certO = pured . emptyToNothing <$>
  strOption
  ( long "certificate"
    <> short 'c'
    <> value ""
    <> help "path to SSL certificate file (./certificate.pem)"
    <> metavar "CERT"
  )
certKeyO :: Parser (Maybe (Tagged CertKey FilePath))
certKeyO = pured . emptyToNothing <$>
  strOption
  ( long "key"
    <> short 'k'
    <> value ""
    <> help "path to key file of SSL certificate (./key.pem)"
    <> metavar "KEY"
  )
