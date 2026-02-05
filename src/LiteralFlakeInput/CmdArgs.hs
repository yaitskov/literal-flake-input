module LiteralFlakeInput.CmdArgs where

import Options.Applicative
import LiteralFlakeInput.Prelude
    ( ($),
      Eq,
      Show,
      Semigroup((<>)),
      Int,
      (=<<),
      Tagged(Tagged),
      MonadIO(..) )


data HttpPort

data CmdArgs
  = RunService
    { httpPortToListen :: Tagged HttpPort Int
    }
  | LiteralFlakeInputVersion
    deriving (Eq, Show)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    serviceP =
      RunService <$> portOption
    cmdp =
      hsubparser
        (  command "run" (infoP serviceP $ "launch the service exposed over HTTP")
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
