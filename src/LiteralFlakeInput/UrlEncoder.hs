{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.UrlEncoder where
import Data.Binary.Builder (fromByteString, toLazyByteString)
import Data.List.NonEmpty qualified as NL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time.Clock ( getCurrentTime )
import Data.Version (showVersion)
import LiteralFlakeInput.Prelude
import Network.HTTP.Types (encodePathSegments)
import Nix
    ( Options,
      nixEvalExprLoc,
      normalForm,
      defaultOptions,
      parseNixTextLoc )
import Nix.Standard ( runWithBasicEffectsIO )
import Paths_literal_flake_input ( version )
import System.Environment.Blank (getEnvDefault)
import Text.Regex.TDFA ( (=~) )


runUrlEncoder :: IO ()
runUrlEncoder = runUrlEncoderWith . fmap toText =<< getArgs

runUrlEncoderWith :: [Text] -> IO ()
runUrlEncoderWith args
  | args `elem` [ [], ["--version"], ["-v"]] =
    putStrLn $ "Literal Flake Input " <> showVersion version
  | args `elem` [[], ["-h"], ["-help"], ["--help"], ["-help"], ["help"]] =
    putStrLn """Literal Flake Input (LFI) URL encoder
      Usage: nix build $(e -attr-name true \\
                         -attr-name2 null \\
                         -an3 12 \\
                         -an4 hello world \\
                         -an5 [1 2] \\
                         -an7 "{ x = 1; y = 2; }" \\
                         -an7 x: x + 1)

      LFI translates arbitrary command line arguments into a Nix attribute set.
      Nix values are verified with hnix interpreter.

      URL prefix can be overriden via environment variable LFI_SITE.

      Project home page https://github.com/yaitskov/literal-flake-input"""
  | length args == 1 = do
    putStrLn """LFI expects "even" number of argument. See help by -h or --help"""
    exitFailure
  | otherwise =
    case fmap (quoteUnquotedString . joinArgValueWords) <$> groupByAttrName args of
      Left e -> putStrLn (toString e) >> exitFailure
      Right m -> do
        time <- getCurrentTime
        let opts = defaultOptions time
        (goodAtrs, badAtrs) <- M.partition isRight <$> M.traverseWithKey (go opts) m
        if null badAtrs
          then do
          siteRoot <- toText <$> getEnvDefault "LFI_SITE" siteRootDef
          putLBSLn . mkLfiUrl siteRoot . M.mapKeys argToAtr $ M.mapMaybe rightToMaybe goodAtrs
          else do
          putStrLn $ "Invalid attribute(s):\n" <> show (M.mapMaybe leftToMaybe  badAtrs)
          exitFailure
  where
    siteRootDef = "https://lficom.me"
    go opts (ArgName an) av = do
      validateNixExpr opts av >>= \case
        Left e -> pure . Left $ "Attribute " <> an <> " " <> e
        Right () -> pure $ Right av

newtype ArgName = ArgName Text deriving newtype (Eq, Show, Ord)
newtype AtrName = AtrName Text deriving newtype (Eq, Show, Ord)

argToAtr :: ArgName -> AtrName
argToAtr (ArgName s) = AtrName (T.drop 1 s)

isAtrName :: Text -> Bool
isAtrName = (=~ ("^[-][a-zA-Z_][a-zA-Z0-9_-]*[a-zA-Z0-9_]*$" :: Text))

groupByAttrName :: [Text] -> Either Text (Map ArgName (NonEmpty Text))
groupByAttrName [] = Right M.empty
groupByAttrName [a] = Left $ "Attribute [" <> a <> "] has no value"
groupByAttrName (atrN:argsLeft)
  | isAtrName atrN =
      case span isAtrName argsLeft of
        (atrVs, nextAtr) ->
          case nonEmpty atrVs of
            Nothing -> Left $ "Attribute [" <> atrN <> "] has no value"
            Just neAtrVs ->
              M.insert (ArgName atrN) neAtrVs <$> groupByAttrName nextAtr
  | otherwise = Left $ "Attribute name [" <> atrN <> "] is not valid"

joinArgValueWords :: NonEmpty Text -> Text
joinArgValueWords =  unwords . NL.toList

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

validateNixExpr :: Options -> Text -> IO (Either Text ())
validateNixExpr o nxe =
  case parseNixTextLoc nxe of
    Left e ->
      pure . Left $ "[" <> nxe <>  "] is bad Nix expression due:" <> show e
    Right e ->
      runWithBasicEffectsIO o $ do
        !_ <- getNormForm e
        pure $ Right ()
  where
    getNormForm = normalForm <=< nixEvalExprLoc mempty

mkLfiUrl :: Text -> Map AtrName Text -> LByteString
mkLfiUrl siteRoot w =
  toLazyByteString $
    "--override-input c " <>
    fromByteString (encodeUtf8 siteRoot) <>
    encodePathSegments (flatpairs $ M.toList w)
  where
   flatpairs :: [(AtrName, Text)] -> [Text]
   flatpairs [] = []
   flatpairs ((AtrName an, av):t) = an : av : flatpairs t
