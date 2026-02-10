module LiteralFlakeInput.Nix where

import Data.Text.Lazy ( concat )
import Codec.Archive.Tar ( write )
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath, TarPath )
import Data.Char ( isDigit )
import Data.Text ( all, null )
import LiteralFlakeInput.Prelude
    ( otherwise,
      ($),
      (<$>),
      Eq,
      Show,
      Semigroup((<>)),
      Bool(False),
      Either(Left, Right),
      Text,
      LazyStrict(toLazy),
      uncurry,
      (.),
      LText,
      (&&),
      not,
      (||),
      error,
      elem,
      ConvertUtf8(encodeUtf8),
      LByteString,
      ToText(toText) )
import Yesod.Core
    ( ToTypedContent(..),
      TypedContent(TypedContent),
      ToContent(..),
      ContentType )

data OutFormat =  TaredNix | PlainNix deriving (Show, Eq)
newtype NixDer (o :: OutFormat)  = NixDer [(Text, Text)] deriving (Show, Eq)

instance ToContent (NixDer PlainNix) where
  toContent = toContent . renderNixDer

instance ToContent (NixDer TaredNix) where
  toContent = toContent . mkTar . renderNixDer

instance ToTypedContent (NixDer PlainNix) where
  toTypedContent = TypedContent xPlainNix . toContent

instance ToTypedContent (NixDer TaredNix) where
  toTypedContent = TypedContent xTarMime . toContent

pairs :: [a] -> [(a, a)]
pairs (a:b:t) = (a, b) : pairs t
pairs [_] = []
pairs []  = []

xTarMime :: ContentType
xTarMime = "application/x-tar"

xPlainNix :: ContentType
xPlainNix = "text/x-nix"

defaultNixName :: TarPath
defaultNixName =
  case toTarPath False "default.nix" of
    Right v -> v
    Left e -> error $ toText e

mkTar :: LText -> LByteString
mkTar txt = write [e]
  where
    e = fileEntry defaultNixName $ encodeUtf8 txt

{- HLINT ignore "Use concatMap" -}
renderNixDer :: NixDer o -> LText
renderNixDer (NixDer m) =
  "{...}:{" <> concat (uncurry renderEntry <$> m) <> "}"

renderEntry :: Text -> Text -> LText
renderEntry k v = toLazy k <> "=" <> tryToQuote v <> ";"
  where
    tryToQuote x
      | notNeedQuotes x = toLazy x
      | otherwise = "\"" <> toLazy x <> "\""

    notNeedQuotes x = isKeyword x || isNumber x
    isKeyword = (`elem` ["null", "true", "false"])
    isNumber x = all isDigit x && not (null x)

translate :: [Text] -> NixDer o
translate = NixDer . pairs
