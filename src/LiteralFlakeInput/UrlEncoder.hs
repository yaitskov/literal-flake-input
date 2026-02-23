{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.UrlEncoder where
import Data.Binary.Builder
    ( Builder, toLazyByteString, fromByteString )
import Data.List.NonEmpty qualified as NL
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time ( UTCTime(UTCTime), fromGregorian )
import Data.Version (showVersion)
import LiteralFlakeInput.Nix
import LiteralFlakeInput.Prelude
import Network.HTTP.Types
    ( decodePath, encodePathSegments, extractPath )
import Nix
import Nix.Atoms ( NAtom(NURI) )
import Nix.Standard ( runWithBasicEffectsIO )
import Paths_literal_flake_input ( version )
import Prettyprinter (layoutSmart, defaultLayoutOptions, PageWidth (..), LayoutOptions (..))
import Prettyprinter.Render.Text (renderStrict)
import System.Environment.Blank (getEnvDefault)
import Text.PrettyPrint.Leijen.Text (linebreak, text, putDoc, vcat, indent)
import Text.Regex.TDFA ( (=~) )
import UnliftIO.Directory ( doesFileExist, makeAbsolute )

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
                           -an7 x: x + 1\\
                           -an8 \\(1 + 2\\) )

      The result is a non-flake URL input:
      nix build --override-input c https://lficom.me/an1/true/an2/null/an3/42/an4/%22hello%20world%22/an5/%5B%201%202%20%5D/an6/%7Bx%20=%201%3B%20y%20=%202%3B%20%7D/an7/a:%20a%20+%201/.tar

      LFI translates arbitrary command line arguments into a Nix attribute set.
      Nix values are verified with hnix interpreter.

      URL prefix can be overriden via environment variable LFI_SITE.
      If you copy URL into flake file as a default value then drop tar suffix.

      Project home page https://github.com/yaitskov/literal-flake-input"""
    exitFailure
  | otherwise =
    case args of
      ("i":initArgs) -> do
         argsMap <- M.mapKeys argToAtr <$> parsePath initArgs
         withUrl argsMap (insertLfiIntoFlake flakeFile)
      ("p":printArgsOverride) -> prettyPrintLfi flakeFile printArgsOverride
      -- ("m":mergeArgs) -> merge with default url before doing init
      _ | length args == 1 -> do
          putStrLn """LFI expects "even" number of argument. See help by -h or --help"""
          exitFailure
        | otherwise -> mergeCmdArgsWithFlakeUrlArgsAndPrintAsUrl flakeFile args
  where
    flakeFile = "./flake.nix"
posixEpoch :: UTCTime
posixEpoch = UTCTime d 0
  where
    d = fromGregorian 1970 1 1

parsePath :: (MonadFail m, MonadIO m) => [Text] -> m (Map ArgName Text)
parsePath args =
  case fmap (quoteUnquotedString . joinArgValueWords) <$> groupByAttrName args of
    Left e -> putStrLn (toString e) >> exitFailure
    Right m -> do
      let opts = defaultOptions posixEpoch
      (goodAtrs, badAtrs) <- M.partition isRight <$> M.traverseWithKey (go opts) m
      if null badAtrs then
        pure $ M.mapMaybe rightToMaybe goodAtrs
      else
        do
          liftIO $ putDoc $ text "Invalid attribute(s):" <> linebreak <>
            idnt4 (vcat . mapMaybe leftToMaybe $ M.elems badAtrs) <>
            linebreak
          exitFailure
  where
    idnt4 = indent 4
    go opts (ArgName an) av = do
      validateNixExpr opts av >>= \case
        Left e -> pure . Left $
          (text (toLazy an) <> linebreak <> (idnt4 . vcat . fmap (text . toLazy) $ lines e))
        Right () -> pure $ Right av

withUrl :: MonadIO m => Map AtrName Text -> (Builder -> m ()) -> m ()
withUrl args cb = do
  siteRoot <- toText <$> liftIO (getEnvDefault "LFI_SITE" siteRootDef)
  cb $ mkLfiUrl siteRoot args
  where
    siteRootDef = "https://lficom.me"

newtype ArgName = ArgName { unArgName :: Text } deriving newtype (Eq, Show, Ord)
newtype AtrName = AtrName { unAtrName :: Text } deriving newtype (Eq, Show, Ord)

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


validateNixExpr :: MonadIO m => Options -> Text -> m (Either Text ())
validateNixExpr o nxe =
  case parseNixTextLoc nxe of
    Left e ->
      pure . Left $ "[" <> nxe <>  "] is bad Nix expression due:" <> show e
    Right e ->
      liftIO $ runWithBasicEffectsIO o $ do
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

prettyPrintLfi :: (MonadFail m, MonadIO m) => FilePath -> [Text] -> m ()
prettyPrintLfi flakeFile printArgsOverride =
  withFlakeInputUrlFromFile flakeFile printUrlAsAtrSet
  where
    layout =
      layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine 20 1 }
    printUrlAsAtrSet url = do
      overMap :: Map Text Text <-  M.mapKeys (unAtrName . argToAtr) <$> parsePath printArgsOverride
      case decodePath . extractPath $ encodeUtf8 url of
        (pathSegs, _qa) ->
          let NixDer bindings = translate @PlainNix pathSegs in
          case parseNixTextLoc . toText . renderNixDer . NixDer . M.toList $ overMap <> M.fromList bindings of
            Left e -> fail $ "Cannot parse attr set of url: " <> show e
            Right atrs -> putTextLn . renderStrict . layout . prettyNix $ stripAnnotation atrs

withFlakeInputUrlFromFile :: (MonadFail m, MonadIO m) => FilePath -> (Text -> m ()) -> m ()
withFlakeInputUrlFromFile flakeFile cb =
  doesFileExist flakeFile >>= \case
  True -> do
    flakeFileContent :: Text <- decodeUtf8 <$> readFileBS flakeFile
    case parseNixTextLoc flakeFileContent of
      Left e -> fail $ "Failed to parse " <> flakeFile <> " due" <> show e
      Right ast ->
        case inputsUrlCBinding ast of
          Nothing ->
            fail $ "Flake file " <> flakeFile <> " does not have input [c] or url of the input is missing"
          Just (Ann _ (NConstant (NURI curUr))) ->
            cb curUr
          Just (Ann _ (NStr (DoubleQuoted [Plain curUr]))) ->
            cb curUr
          Just (Ann _ unsupported) ->
            fail $ "c input url is not a string but: " <> show unsupported
  False -> do
    absFf <- makeAbsolute flakeFile
    fail $ "Flake file [" <> absFf <> "] does not exist"


mergeCmdArgsWithFlakeUrlArgsAndPrintAsUrl :: (MonadFail m, MonadIO m) => FilePath -> [Text] -> m ()
mergeCmdArgsWithFlakeUrlArgsAndPrintAsUrl flakeFile cmdArgsOverride =
  withFlakeInputUrlFromFile flakeFile printUrlAsAtrSet
  where
    printUrlAsAtrSet url = do
      overMap <-  M.mapKeys argToAtr <$> parsePath cmdArgsOverride
      case decodePath . extractPath $ encodeUtf8 url of
        (pathSegs, _qa) ->
          let
            NixDer bindings = translate @PlainNix pathSegs
            argsFromFlake :: Map AtrName Text = M.mapKeys AtrName $ M.fromList bindings
          in
            withUrl
              (overMap <> argsFromFlake)
              (liftIO . putLBSLn . toLazyByteString . ( "--override-input c " <>) . (<> ".tar"))

insertLfiIntoFlake :: (MonadFail m, MonadIO m) => FilePath -> Builder -> m ()
insertLfiIntoFlake flakeFile url =
  doesFileExist flakeFile >>= \case
    True -> updateFlakeFile
    False -> do
      absFf <- makeAbsolute flakeFile
      fail $ "Flake file [" <> absFf <> "] does not exist"
  where
    updateFlakeFile = do
      flakeFileContent :: Text <- decodeUtf8 <$> readFileBS flakeFile
      case parseNixTextLoc flakeFileContent of
        Left e -> fail $ "Failed to parse " <> flakeFile <> " due" <> show e
        Right ast ->
          case inputsUrlCBinding ast of
            Nothing ->
              case inputsFirstBindingPos ast of
                Nothing -> fail $ "Flake ["  <> flakeFile <> "] does not have inputs"
                Just inputsPos ->
                  writeFileText flakeFile $
                    insertInputC
                      (renderInputsEntry (snd inputsPos) (decodeUtf8 $ toLazyByteString url))
                      (fst inputsPos)
                      flakeFileContent
            Just oldUrl ->
              writeFileText flakeFile $
                insertUrlCInput
                  oldUrl
                  (mkConst (NURI (decodeUtf8 $ toLazyByteString url)))
                  flakeFileContent
