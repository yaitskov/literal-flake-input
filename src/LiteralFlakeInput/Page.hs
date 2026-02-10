{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.Page where

import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed ( embedFile, makeRelativeToProject )
import Data.Text (isSuffixOf)
import LiteralFlakeInput.Nix
    ( translate, OutFormat(PlainNix, TaredNix) )
import LiteralFlakeInput.Prelude
import Yesod.Core
    ( Yesod(defaultLayout, makeSessionBackend),
      RenderRoute(renderRoute),
      ToTypedContent(..),
      TypedContent(TypedContent),
      Html,
      Content(ContentBuilder),
      ToContent(..),
      ToWidgetHead(toWidgetHead),
      preEscapedToMarkup,
      Texts,
      mkYesod,
      parseRoutes,
      logInfo,
      addContentDispositionFileName,
      setTitle,
      hamlet,
      lucius,
      whamlet,
      typeSvg )

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

newtype FavIcon = FavIcon ByteString

instance ToContent FavIcon where
  toContent (FavIcon bs) =
    ContentBuilder (fromByteString bs) (Just . fromIntegral $ BS.length bs)
instance ToTypedContent FavIcon where
  toTypedContent = TypedContent typeSvg . toContent

getFavIcon :: Handler FavIcon
getFavIcon = pure $ FavIcon $(makeRelativeToProject "assets/favicon.svg" >>= embedFile)

getHomeR :: Texts -> Handler Contentable
getHomeR params = do
  $(logInfo) $ "Translate " <> show params
  case nonEmpty params of
    Nothing -> Contentable <$> landing
    Just p | ".tar" `isSuffixOf` last p ->
               addContentDispositionFileName "file.tar" $> Contentable (translate @TaredNix params)
           | p == pure "favicon.svg" ->
               Contentable <$> getFavIcon
           | otherwise ->
               addContentDispositionFileName "default.nix" $> Contentable (translate @PlainNix params)

generatedNixExpr :: Text
generatedNixExpr =
  """
  { ... }:
    {
      name  = "Alice Wonder";
      age   = 18;
      alive = true;
      job   = null;
    }
  """

nixFlakeTemplate :: Text
nixFlakeTemplate =
  """
  # ...
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    c = {
      url = "https://lficom.me/static/false/";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, c }:
    flake-utils.lib.eachDefaultSystem (sys:
      let
        cnf = import c { inherit pkgs; };
      in
      {
        packages.${packageName} =
          pkgs.writeShellScriptBin
            "hello"
            "echo Hello ${cnf.name}";
      });
  # ...
  """

nixBuildWithOverride :: Text
nixBuildWithOverride =
  """
  nix build --override-input c \\
    https://lficom.me/name/Bob/.tar
  """

landing :: Handler Html
landing =
  defaultLayout $ do
    setTitle "VPN Router"
    toWidgetHead
      [hamlet|
             <meta charset="utf-8" />
             <meta name="viewport" content="width=device-width, initial-scale=1" />
             <meta name="author" content="Daniil Iaitskov" />
             <meta name="keywords" content="nix flake input arguments nixpkgs derivation" />
             <meta name="description" content="Micro-service helps to pass argments to nix flakes as in nixpkgs" />
             <link rel="shortcut icon" href="favicon.svg" type="image/x-icon">
             |]
    toWidgetHead
      [lucius|
             body {
                 background: #7CED53;
                 background: radial-gradient(circle, rgb(182 213 217) 0%, rgb(205 255 253) 80%, rgb(206 214 255) 100%);
                 padding: 0px 4%;
                 color: #171717;
             }
             h1 a {
               font-size: x-large;
               text-decoration: none;
               color: #e64d4d;
             }
             pre {
                 font-size: small;
             }
             .wrap {
                 white-space: pre-wrap;
                 text-indent: -2em;
                 padding-left: 2em;
             }
             |]
    [whamlet|
            <h1>
              <a href="https://lficom.me">
                Literal Flake Input
            <p>
              Service translates an HTTP GET path into a nix derivation
              that can be used as a flake input. Such a workaround provides
              the ability to emulate command line arguments in nix flakes.
            <pre class=wrap>
              wget -q -O - 'https://lficom.me/name/Alice Wonder/age/18/alive/true/job/null/'
            <h5>
              Response body
            <pre>
              #{preEscapedToMarkup generatedNixExpr}

            <p>
              nixpkgs derivations can accept command line arguments but nix flakes don't.
            <h5>
              Flake template
            <pre>
              #{preEscapedToMarkup nixFlakeTemplate}
            <h5>
              Override
            <pre>
              #{preEscapedToMarkup nixBuildWithOverride}

            <h3>Project repository
            <p>
              <a href="https://github.com/yaitskov/literal-flake-input">
                GitHub
            |]
