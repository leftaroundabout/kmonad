{-# LANGUAGE DeriveAnyClass #-}
{-| TODO: do this

-}
module K.Shell.Cfg.CfgFile where

import K.Initial.Parsing
-- import K.Gesture
import K.Shell.Cfg.Initial
import K.Shell.Cfg.Expr
import K.Shell.Cfg.Cfgable

import qualified Dhall as D
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.HashMap as M

import qualified Control.Monad.Error.Lens as Err

-- basic types -----------------------------------------------------------------

-- | An entry in a Dhall Map
data DEntry k v = DEntry
  { mapKey :: k
  , mapValue :: v
  } deriving (Generic, D.FromDhall, Show)

-- | A Dhall Map value
type DMap k v = [ DEntry k v ]

-- | Use '_DMap' as a view of an alist as a DMap, and 'from _DMap' as its inverse
_DMap :: Iso' [(k, v)] (DMap k v)
_DMap = iso (map $ uncurry DEntry) (map $ \(DEntry k v) -> (k, v))

instance HasNames (DMap Name v) where names = from _DMap . folded . _1


-- | The entire Dhall configuration
data CfgFile = CfgFile
  { _keycodes :: DMap Name Keycode -- ^ Name to keycode-number alist
  , _gestures :: DMap Name Text    -- ^ Name to gesture expression alist
  , _options  :: DMap Name Text    -- ^ Name to option expression alist
  , _flags :: [Text]               -- ^ List of flag expressions
  } deriving (Generic, D.FromDhall, Show)
makeLenses ''CfgFile

-- errors ----------------------------------------------------------------------

data CfgFileError
  = CfgNameError      NameError    -- ^ Either an empty name or duplicate names
  | CfgExprError      ExprError    -- ^ Error while parsing an Expr somewhere
  | CfgUnknownFlag    Name         -- ^ Reference to unknown flag
  | CfgUnknownOption  Name         -- ^ Reference to unknown option
  | CfgMissingKeyname Name         -- ^ Reference to non-existent keyname in gesture
  | CfgGestureError   GestureError -- ^ Config contains some invalid gesture
makeClassyPrisms ''CfgFileError

instance Show CfgFileError where
  show (CfgNameError e) =
    "NameError while reading CfgFile: " <> show e
  show (CfgExprError e) =
    "ExprError while reading CfgFile: " <> show e
  show (CfgUnknownFlag n) =
    "Unknown flag: " <> unpack n
  show (CfgUnknownOption n) =
    "Unknown option: " <> unpack n
  show (CfgMissingKeyname n) =
    "Reference to undefined keyname in CfgFile: " <> show n
  show (CfgGestureError e) =
    "GestureError while reading CfgFile: " <> show e

instance Exception CfgFileError
instance AsCfgFileError SomeException where _CfgFileError = _SomeException
instance AsGestureError CfgFileError where _GestureError = _CfgGestureError

-- IO --------------------------------------------------------------------------

-- | Try to load a 'CfgFile' from disk
loadCfgFile :: MonadIO m => Path -> m CfgFile
loadCfgFile p = do
  pth <- resolve p
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
  let dec = D.genericAutoWith opt
  liftIO $ D.inputFile dec pth

-- validation ------------------------------------------------------------------

-- | Take all the fields in a 'CfgFile' and make a valid 'Change AppCfg'
validateCfgFile :: (AsCfgFileError e, MonadError e m)
  => CfgFile -> m (Change AppCfg)
validateCfgFile c = do

  -- Validate names, checking for any empty's or duplicates
  let allNames = c^..keycodes.names <> c^..gestures.names
  case L.find T.null allNames of
    Nothing -> pure ()
    Just _  -> Err.throwing (_CfgNameError . _EmptyName) ()
  case duplicates allNames of
    [] -> pure ()
    ns -> Err.throwing (_CfgNameError . _DuplicateNames) ns
  let namedCodes = M.fromList $ c^.keycodes.from _DMap

  -- Extract and validate flags by looking them up and extracting their change
  fchange <- flip foldMapM (c^.flags) $ \flag -> do
    case lookupLong flag appFlags of
      Nothing -> Err.throwing _CfgUnknownFlag flag
      Just x  -> pure $ x^.change

  -- Extract and validate options by looking them up and calling their
  -- mkChange function on the provided Text value in the CfgFile
  ochange <- flip foldMapM (c^.options.from _DMap) $ \(okey, oval) -> do
    case lookupLong okey appOptions of
      Nothing -> Err.throwing _CfgUnknownOption okey
      Just x  -> case (x^.mkChange) oval of
        Left e -> Err.throwing _CfgExprError e
        Right y -> pure y

  -- Extract all 'Toggles' expressions
  togTxt <- forM (c^.gestures.from _DMap) $ \(n, t) ->
    case decode togglesExpr t of
      Left e -> Err.throwing _CfgExprError e
      Right x -> pure (n, x)

  -- Convert all 'Toggles Keyname' to 'Toggles Keycode'
  togNat <- forM togTxt $ \(n, t) ->
    case togglesMapMaybe (`M.lookup` namedCodes) t of
      Left n  -> Err.throwing _CfgMissingKeyname n
      Right x -> pure (n, x)

  -- Convert all 'Toggles Keycode' to proper 'Gesture Keycode'
  gests <- forM togNat $ \(n, t) -> case mkGesture t of
    Left e -> Err.throwing _CfgGestureError e
    Right x -> pure (n, x)

  -- Gather all the codes and gestures into a LocaleCfg
  let loc = LocaleCfg
        { _namedCodes = namedCodes
        , _namedGestures = M.fromList gests }

  -- Gather all LocaleCfg, options, and flags into 1 update to AppCfg
  pure . mconcat $
    [ setVal localeCfg loc "set keynames and gesturenames"
    , fchange
    , ochange ]


-- ops -------------------------------------------------------------------------

-- | Try to translate the contents of a traversable, on error return failing key
--
-- This is a very general way of writing code that goes into a Gesture and
-- performs the (Name -> Keycode) mapping.
--
-- A more concrete way of writing this signature would be:
--
-- coded :: (Name -> Maybe Keycode) -> Gesture Name -> Either Name (Gesture Keycode)
--
-- coded :: Traversable t => (b -> Maybe a) -> t b -> Either b (t a)
-- coded f x = sequence $ (\n -> maybe (Left n) Right (f n)) <$> x

-- | Contruct a valid LocaleCfg or return an error detailing an issue.
-- getLocale :: (MonadError e m, AsCfgFileError e) => CfgFile -> m LocaleCfg
-- getLocale  c = do
--   let cl = c^.keycodes.from _DMap
--   let gl = c^.gestures.from _DMap

--   -- Create the keycode map
--   let codes = M.fromList cl

--   -- Check for valid gesture structure
--   let (e1, gtxt) = partitionEithers . map readGesture $ gl^..folded._2
--   unless (L.null e1) $ Err.throwing _CfgExprError e1
--     -- throwError (_LocaleGestureError # e1)

--   -- Check that gestures only refer to existing keynames
--   let (e2, gcode) = partitionEithers . over mapped (coded (`M.lookup` codes)) $ gtxt
--   unless (L.null e2) $ throwError (_CfgMissingKeyname # e2)

--   -- Put it all together
--   pure . LocaleCfg codes . M.fromList . zip (gl^..folded._1) $ gcode


-- data CfgError = CfgLocaleError LocaleError deriving (Eq, Show)
-- makeClassyPrisms ''CfgError

-- instance Exception CfgError
-- instance AsCfgError SomeException where _CfgError = exception
-- instance AsLocaleError CfgError where _LocaleError = _CfgLocaleError

-- -- | Extract the full 'Change AppCfg' from a 'CfgFile'
-- getChange :: (AsCfgFileError e, MonadError e m) => CfgFile -> m (Change AppCfg)
-- getChange c = do
--   l <- getLocale c
--   pure undefined
