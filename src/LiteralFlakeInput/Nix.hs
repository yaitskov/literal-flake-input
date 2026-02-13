module LiteralFlakeInput.Nix where

import Data.Text.Lazy ( concat )
import Codec.Archive.Tar ( write )
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath, TarPath )
import LiteralFlakeInput.Prelude
    ( otherwise,
      show,
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
      error,
      ConvertUtf8(encodeUtf8),
      LByteString,
      Bool(True),
      ToText(toText) )
import Text.Regex.TDFA ( (=~) )
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

isUnquotedString :: Text -> Bool
isUnquotedString s
  | s =~ ("^(true|false|null|-?[0-9]+([.][0-9]+)?|\".*\"|[[].*|[{].*|[a-zA-Z_][a-zA-Z0-9_-]*[a-zA-Z0-9_]*:.*)$" :: Text) = False
  | otherwise = True

quoteString :: Text -> Text
quoteString = show

quoteUnquotedString :: Text -> Text
quoteUnquotedString s
  | isUnquotedString s = quoteString s
  | otherwise = s

renderEntry :: Text -> Text -> LText
renderEntry k v = toLazy k <> "=" <> toLazy (quoteUnquotedString v) <> ";"

translate :: [Text] -> NixDer o
translate = NixDer . pairs
