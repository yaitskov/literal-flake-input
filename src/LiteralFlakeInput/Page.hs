{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.Page where

import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed ( embedFile, makeRelativeToProject )
import Data.Text (isSuffixOf)
import Data.Version (showVersion)
import LiteralFlakeInput.Nix
    ( translate, OutFormat(PlainNix, TaredNix), NixDer(..))
import LiteralFlakeInput.Prelude
import Paths_literal_flake_input ( version )
import System.Metrics ( Store )
import Yesod.Core
    ( Yesod(defaultLayout, makeSessionBackend),
      notFound,
      getYesod,
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
      typeXml,
      typePlain,
      typeSvg, notFound )

import System.Metrics.Counter ( Counter, inc )

data YppMetrics
  = YppMetrics
  { landingPage :: Counter
  , apiCall :: Counter
  }

data Ypp
  = Ypp
    { metricsStore :: Store
    , yppMetrics :: YppMetrics
    }

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

getGithubIcon :: Handler FavIcon
getGithubIcon = pure $ FavIcon $(makeRelativeToProject "assets/github.svg" >>= embedFile)

getSiteMap :: Handler TypedContent
getSiteMap = pure . TypedContent typeXml $ toContent $(makeRelativeToProject "assets/sitemap.xml" >>= embedFile)

getRobots :: Handler TypedContent
getRobots = pure . TypedContent typePlain $ toContent $(makeRelativeToProject "assets/robots.txt" >>= embedFile)

getHomeR :: Texts -> Handler Contentable
getHomeR params =
  case nonEmpty params of
    Nothing -> Contentable <$> landing
    Just (p :| []) ->
      case p of
        "favicon.svg" -> Contentable <$> getFavIcon
        "github.svg" -> Contentable <$> getGithubIcon
        "robots.txt" ->  Contentable <$> getRobots
        "sitemap.xml" -> Contentable <$> getSiteMap
        "version" ->
          pure . Contentable . TypedContent typePlain . toContent $
          "Literal Flake Input " <> showVersion version
        "index.html" ->  Contentable <$> landing
        _ -> notFound
    Just p | ".tar" `isSuffixOf` last p ->
               trans (Proxy @TaredNix) "file.tar"
           | otherwise ->
               trans (Proxy @PlainNix) "default.nix"
  where
    trans :: forall tw. ToTypedContent (NixDer tw) => Proxy tw -> Text -> Handler Contentable
    trans _ fileName = do
      yp <- getYesod
      liftIO (inc $ apiCall $ yppMetrics yp)
      $(logInfo) $ "Translate " <> show params
      addContentDispositionFileName fileName $> Contentable (translate params :: NixDer tw)

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

nixAndE :: Text
nixAndE = """nix build $(e -name Bob)"""

fullTypeRangeOfE :: Text
fullTypeRangeOfE =
  """
  nix build $(e -an1 true \\
                -an2 null \\
                -an3 12 \\
                -an4 hello world \\
                -an5 [ 1 2 ] \\
                -an6 "{x = 1; y = 2; }" \\
                -an7 x: x + 1)
  """

fullTypeRangeInputBody :: Text
fullTypeRangeInputBody =
  """
  {...}: {
    an1 = true;
    an2 = null;
    an3 = 12;
    an4 = "hello wolrd";
    an5 = [1 2];
    an6 = {x = 1; y = 2; };
    an7 = x: x + 1;
  }
  """

nixFlakeTemplate :: Text
nixFlakeTemplate =
  """
  # ...
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    utils.url = "github:numtide/flake-utils";
    c = {
      url = "https://lficom.me/static/false/";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, utils, c }:
    utils.lib.eachDefaultSystem (sys:
      let
        pkgs = nixpkgs.legacyPackages.${sys};
        cnf = import c { inherit pkgs; };
      in
      {
        packages.hello =
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
landing = do
  yp <- getYesod
  liftIO (inc $ landingPage $ yppMetrics yp)
  defaultLayout $ do
    setTitle "Literal Flake Input"
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
             img.github {
                 width: 8vh;
                 opacity: 0.8;
                 padding-top: 5vh;
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
              Service translates an HTTP GET path into a Nix attrset
              that can be used as a non-flake input. Such a workaround provides
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
            <p>
              There is a commant line tool <b>e</b>, that helps with boilerplates and
              input validation:
            <pre>
              #{preEscapedToMarkup nixAndE}
            <p>
              The tool supports literal keyword values (e.g. <i>true</i> and <i>null</i>),
              strings, numbers, arrays and attribute sets.  String quotation is
              optional. All values are parsed and evaluated by <b>e</b> with nix to catch typos
              as soon as possible.
            <pre>
              #{preEscapedToMarkup fullTypeRangeOfE}
            <p>
              The above command generates an input link which is going to be resolved
              by the service into:
            <pre>
              #{preEscapedToMarkup fullTypeRangeInputBody}
            <p>
              If you copy an URL generated by <b>e</b> into a flake for default
              values, then drop <i>.tar</i> suffix.
            <p>
              Alternative URL prefix can be set via environment variable <i>LFI_SITE</i>.
            <p>
              <b>e i</b> - init command can initialize default flake input URL just in a flake file.
              So manuall URL handling is not need.
            <p>
              Use <b>e p</b> - to print Nix attrset isomophic to flake input URL.
            <p>
              <b>e m</b> versus from init updates only specified attributes in the flake input URL.
            <p>
              <b>e x</b> deletes specified attributes from the flake input URL.

            <center>
              <p>
                <a href="https://github.com/yaitskov/literal-flake-input">
                  <img class=github src=/github.svg />

            |]
