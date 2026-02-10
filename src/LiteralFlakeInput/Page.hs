{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module LiteralFlakeInput.Page where

import LiteralFlakeInput.App ( ex )
import LiteralFlakeInput.Nix
    ( translate, OutFormat(PlainNix, TaredNix) )
import Data.Text (isSuffixOf)
import LiteralFlakeInput.Prelude
    ( ($), Applicative(pure), Semigroup((<>)), show, Maybe(..), last, otherwise, nonEmpty, ($>) )
import Yesod.Core
    ( Yesod(..),
      RenderRoute(renderRoute),
      Texts,
      mkYesod,
      parseRoutes,
      logInfo,
      addContentDispositionFileName, ToTypedContent (toTypedContent), ToContent (..) )

data Ypp = Ypp

mkYesod "Ypp" [parseRoutes|
/*{Texts} HomeR GET
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

data Contentable = forall x. (ToContent x, ToTypedContent x) => Contentable x
instance ToContent Contentable where
  toContent (Contentable x) = toContent x

instance ToTypedContent Contentable where
  toTypedContent (Contentable x) = toTypedContent x

getHomeR :: Texts -> Handler Contentable
getHomeR params = do
  $(logInfo) $ "Translate " <> show params
  case nonEmpty params of
    Nothing -> ex "here must be landing page"
    Just p | ".tar" `isSuffixOf` last p ->
               addContentDispositionFileName "file.tar" $> Contentable (translate @TaredNix params)
           | otherwise ->
               addContentDispositionFileName "default.nix" $> Contentable (translate @PlainNix params)
