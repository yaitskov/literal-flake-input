{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module LiteralFlakeInput.Page where

import LiteralFlakeInput.Nix ( NixDer, translate )
import LiteralFlakeInput.Prelude
    ( ($), Applicative(pure), Semigroup((<>)), show, Maybe(..) )
import Yesod.Core
    ( Yesod(..),
      RenderRoute(renderRoute),
      Texts,
      mkYesod,
      parseRoutes,
      logInfo,
      addContentDispositionFileName )

data Ypp = Ypp

mkYesod "Ypp" [parseRoutes|
/*{Texts} HomeR GET
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

getHomeR :: Texts -> Handler NixDer
getHomeR params = do
  $(logInfo) $ "Translate " <> show params
  addContentDispositionFileName "file.tar"
  pure $ translate params
