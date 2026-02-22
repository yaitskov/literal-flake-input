module LiteralFlakeInput.Nix where

import Prettyprinter ( indent, line )
import Data.Text.Lazy ( concat )
import Data.Text.Zipper
    ( getText, insertMany, moveCursor, textZipper )
import Codec.Archive.Tar ( write )
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath, TarPath )
import LiteralFlakeInput.Prelude hiding (concat)
import Nix
import Nix.Atoms ( NAtom(NURI) )
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
  | s =~ ("^(true|false|null|-?[0-9]+([.][0-9]+)?|\".*\"|[[].*[]]|[{].*[}]|[(].*[)]|[a-zA-Z_][a-zA-Z0-9_-]*[a-zA-Z0-9_]*:.*)$" :: Text) = False
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


lookupBindings :: Ann ann NExprF -> Maybe [Binding (Ann ann NExprF)]
lookupBindings = \case (Ann _ (NSet _ b)) -> pure b ; _ -> Nothing

lookupInputsBinding :: [Binding NExprLoc] -> Maybe NSourcePos
lookupInputsBinding =
  \case
    [] -> Nothing
    ((NamedVar (StaticKey (VarName "inputs") :| []) x _):_) ->
      case x of
        (Ann _ (NSet _ (NamedVar _ _ p : _))) -> pure p
        (Ann p _empty) -> pure $ getSpanBegin p
    (_:r) -> lookupInputsBinding r

inputsFirstBindingPos :: NExprLoc -> Maybe (Int, Int)
inputsFirstBindingPos = fmap fixBasis <$> lookupInputsBinding <=< lookupBindings

fixBasis :: NSourcePos -> (Int, Int)
fixBasis (NSourcePos _ l c) = (decPos l, decPos c)

decPos :: NPos -> Int
decPos (NPos x) = max 0 $ unPos x - 1

nposToI :: NPos -> Int
nposToI (NPos x) = unPos x

insertInputC :: Text -> Int -> Text -> Text
insertInputC snippet pos =
  unlines . getText . insertMany snippet .
    moveCursor (pos, 0) .
       (`textZipper` Nothing) . lines

renderInputsEntry :: Int -> Text -> Text
renderInputsEntry i url =
  show . indent i $ "c = " <> prettyNix ne <> ";" <> line
  where
    ne = mkNonRecSet
      [ NamedVar
          (StaticKey "url" :| [])
          (mkConst (NURI url))
          nullPos
      , NamedVar
          (StaticKey "flake" :| [])
          (mkBool False)
          nullPos
      ]
