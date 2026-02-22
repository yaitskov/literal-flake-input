{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.UrlEncoder where
import Data.Binary.Builder -- (fromByteString, toLazyByteString)
import Data.List.NonEmpty qualified as NL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time.Clock ( getCurrentTime )
import Data.Version (showVersion)
import LiteralFlakeInput.Nix
    ( quoteUnquotedString,
      inputsFirstBindingPos,
      insertInputC,
      renderInputsEntry )
import LiteralFlakeInput.Prelude
import Network.HTTP.Types (encodePathSegments)
import Nix
    ( nixEvalExprLoc,
      normalForm,
      defaultOptions,
      parseNixTextLoc,
      Options )
import Nix.Standard ( runWithBasicEffectsIO )
import Paths_literal_flake_input ( version )
import System.Environment.Blank (getEnvDefault)
import Text.PrettyPrint.Leijen.Text (linebreak, text, putDoc, vcat, indent)
import Text.Regex.TDFA ( (=~) )

runUrlEncoder :: IO ()
runUrlEncoder = runUrlEncoderWith . fmap toText =<< getArgs

runUrlEncoderWith :: [Text] -> IO ()
runUrlEncoderWith args
  | args `elem` [["--version"], ["-v"]] = do
    putStrLn $ "Literal Flake Input " <> showVersion version
    exitFailure
  | args `elem` [[], ["-h"], ["-help"], ["--help"], ["-help"], ["help"]] = do
    putStrLn """Literal Flake Input (LFI) URL encoder
      Usage: nix build $(e -an1 true \\
                           -an2 null \\
                           -an3 12 \\
                           -an4 hello world \\
                           -an5 [ 1 2 ] \\
                           -an6 "{x = 1; y = 2; }" \\
                           -an7 x: x + 1)

      The result is a non-flake URL input:
      nix build --override-input c https://lficom.me/an1/true/an2/null/an3/42/an4/%22hello%20world%22/an5/%5B%201%202%20%5D/an6/%7Bx%20=%201%3B%20y%20=%202%3B%20%7D/an7/a:%20a%20+%201/.tar

      LFI translates arbitrary command line arguments into a Nix attribute set.
      Nix values are verified with hnix interpreter.

      URL prefix can be overriden via environment variable LFI_SITE.
      If you copy URL into flake file as a default value then drop tar suffix.

      Project home page https://github.com/yaitskov/literal-flake-input"""
    exitFailure
  | length args == 1 = do
    putStrLn """LFI expects "even" number of argument. See help by -h or --help"""
    exitFailure
  | otherwise =
    case args of
      ("i":initArgs) -> withUrl initArgs (insertLfiIntoFlake "./flake.nix")
      _ -> withUrl args (putLBSLn . toLazyByteString . ( "--override-input c " <>) . (<> ".tar"))

withUrl :: [Text] -> (Builder -> IO ()) -> IO ()
withUrl args cb =
  case fmap (quoteUnquotedString . joinArgValueWords) <$> groupByAttrName args of
    Left e -> putStrLn (toString e) >> exitFailure
    Right m -> do
      time <- getCurrentTime
      let opts = defaultOptions time
      (goodAtrs, badAtrs) <- M.partition isRight <$> M.traverseWithKey (go opts) m
      if null badAtrs then
        do
          siteRoot <- toText <$> getEnvDefault "LFI_SITE" siteRootDef
          cb . mkLfiUrl siteRoot . M.mapKeys argToAtr $ M.mapMaybe rightToMaybe goodAtrs
        else
        do
          putDoc $ text "Invalid attribute(s):" <> linebreak <>
            idnt4 (vcat . mapMaybe leftToMaybe $ M.elems badAtrs) <>
            linebreak
          exitFailure
  where
    idnt4 = indent 4
    siteRootDef = "https://lficom.me"
    go opts (ArgName an) av = do
      validateNixExpr opts av >>= \case
        Left e -> pure . Left $
          (text (toLazy an) <> linebreak <> (idnt4 . vcat . fmap (text . toLazy) $ lines e))
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
      case break isAtrName argsLeft of
        (atrVs, nextAtr) ->
          case nonEmpty atrVs of
            Nothing -> Left $ "Attribute [" <> atrN <> "] has no value"
            Just neAtrVs ->
              M.insert (ArgName atrN) neAtrVs <$> groupByAttrName nextAtr
  | otherwise = Left $ "Attribute name [" <> atrN <> "] is not valid"

joinArgValueWords :: NonEmpty Text -> Text
joinArgValueWords =  unwords . NL.toList


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

mkLfiUrl :: Text -> Map AtrName Text -> Builder
mkLfiUrl siteRoot w =
  fromByteString (encodeUtf8 siteRoot) <>
  encodePathSegments (flatpairs $ M.toList w)
  where
   flatpairs :: [(AtrName, Text)] -> [Text]
   flatpairs [] = []
   flatpairs ((AtrName an, av):t) = an : av : flatpairs t

insertLfiIntoFlake :: (MonadFail m, MonadIO m) => FilePath -> Builder -> m ()
insertLfiIntoFlake flakeFile url = do
  flakeFileContent :: Text <- decodeUtf8 <$> readFileBS flakeFile
  case parseNixTextLoc flakeFileContent of
    Left e -> fail $ "Failed to parse " <> flakeFile <> " due" <> show e
    Right ast ->
      case inputsFirstBindingPos ast of
        Nothing -> fail $ "Flake ["  <> flakeFile <> "] does not have inputs"
        Just inputsPos -> do
          putStrLn $ "POS " <> show inputsPos
          writeFileText flakeFile $
            insertInputC
              (renderInputsEntry (snd inputsPos) (decodeUtf8 $ toLazyByteString url))
              (fst inputsPos)
              flakeFileContent
