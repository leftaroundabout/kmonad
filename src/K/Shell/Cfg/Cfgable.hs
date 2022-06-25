-- |

module K.Shell.Cfg.Cfgable where
-- |


import K.Initial
import K.Initial.Parsing (ParseError)
import K.Shell.Cfg.Initial
import K.Shell.Cfg.Expr

import Data.Monoid
import Data.Foldable

import qualified Control.Monad.Error.Lens as Err

-- types -----------------------------------------------------------------------

-- | A modification to some structure
data Change s = Change
  { _notes :: [Text]
  , _endo  :: Endo s
  }
makeClassy ''Change

-- | Run a change on some structure
runChange :: Change s -> s -> s
runChange = appEndo . view endo

instance Semigroup (Change s) where
  a <> b = Change (a^.notes <> b^.notes) (a^.endo <> b^.endo)
instance Monoid (Change s) where
  mempty = Change [] (Endo id)

-- | Create a 'Change' that sets a 'Traversal' to some value
setVal :: Traversal' s a -> a -> Text -> Change s
setVal l a t = Change [t] (Endo $ \s -> set l a s)

-- | Different locations a setting may be provided
data CfgSource = FromInvoc | FromCfgFile | FromEither
makeClassyPrisms ''CfgSource

-- | Identifying information about some 'Cfgable'
data CfgTag = CfgTag
  { _longName  :: Name       -- ^ The full name of a 'Cfgable'
  , _shortName :: Maybe Char -- ^ Short character for invocation
  , _doc       :: Text       -- ^ Help text for invocation and in-code documentation
  , _source    :: CfgSource  -- ^ Where to allow specification
  }
makeClassy ''CfgTag

-- | Explicitly mark a configable as invoc only
invocOnly :: HasCfgTag a => a -> a
invocOnly = set source FromInvoc

-- | Explicitly mark a configable as cfg-file only
cfgFileOnly :: HasCfgTag a => a -> a
cfgFileOnly = set source FromCfgFile

-- | A fixed change to the 'AppCfg'
data Flag s = Flag
  { _ftag :: CfgTag  -- ^ Identifying information
  , _fchange :: Change s   -- ^ Change to make to 'AppCfg'
  }
makeClassy ''Flag

instance HasCfgTag (Flag s) where cfgTag = ftag
instance HasChange (Flag s) s where change = fchange

-- | Create a flag that is valid for both 'CfgSource's
mkFlag :: ()
  => Name       -- ^ Long name
  -> Maybe Char -- ^ Optional short name for invocation
  -> Text       -- ^ Help text
  -> Change s      -- ^ The change to make to a structure
  -> Flag s
mkFlag n c t = Flag (CfgTag n c t FromEither)

-- | A change to 'AppCfg' that depends on some value.
data Option e s = Option
  { _otag  :: CfgTag        -- ^ Identifying information
  , _mkChange :: Text -> Either e (Change s) -- ^ How to construct a change to the 'AppCfg'
  }
makeClassy ''Option

instance HasCfgTag (Option e s) where cfgTag = otag

-- | Create an option that is valid for both 'CfgSource's
mkOption :: ()
  => Name            -- ^ Long name
  -> Maybe Char      -- ^ Optional short name for invocation
  -> Text            -- ^ Help text
  -> (Text -> Either e (Change s)) -- ^ How to update the structure with the value
  -> Option e s
mkOption n c t = Option (CfgTag n c t FromEither)

-- | Create a function that sets some value using an 'Expr'
setWithExpr :: (Show a, MonadError e m, AsExprError e)
  => Traversal' s a
  -> Expr a
  -> Name
  -> (Text -> m (Change s))
setWithExpr l e n t = case decode e t of
  Left err -> Err.throwing __ExprError err
  Right a  -> pure $ setVal l a ("set " <> n <> " to " <> tshow a)

-- util ------------------------------------------------------------------------

class HasCfgChange a where cfgChange :: Getter a (Change AppCfg)
instance HasCfgChange (Change AppCfg) where cfgChange = id

-- | Lookup a flag by its long name
lookupLong :: (Foldable f, HasCfgTag a) => Name -> f a -> Maybe a
lookupLong n = find (\f -> f^.longName == n)

-- values ----------------------------------------------------------------------

type AppFlag = Flag AppCfg
type AppOption = Option ExprError AppCfg

appFlags :: [Flag AppCfg]
appFlags =
  [ mkFlag "fallthrough" (Just 'T')
      "Enable unaltered reemission of uncaught events."
      $ setVal fallthrough True "set fallthrough to True"

  , mkFlag "log-to-stderr" (Just 'E')
      "Write log-messages to stderr instead of stdout"
      $ setVal logTarget stderr "set logTarget to stderr"

  , invocOnly $ mkFlag "verbose" (Just 'v')
      "Make logging very verbose"
      $ setVal logLevel LevelDebug "set logLevel to LevelDebug"
  ]

appOptions :: [Option ExprError AppCfg]
appOptions =
  [ -- RunCfg ------------------------------------------------------------------

    invocOnly $ mkOption "run-type" (Just 'r')
      "Task to execute: run | test | discover"
      $ setWithExpr runType runTypeExpr "runType"

  , invocOnly $ mkOption "cfg-path" (Just 'f')
      "Path to dhall-syntax configuration file"
      $ setWithExpr cfgPath pathExpr "cfgPath"

  , mkOption "kbd-path" (Just 'k')
      "Path to klang-syntax keymap file"
      $ setWithExpr kbdPath pathExpr "kbdPath"

  , mkOption "cmd-allow" (Just 'c')
      "What commands to allow: none | init | all"
      $ setWithExpr cmdAllow cmdAllowExpr "cmdAllow"

    -- LogCfg ------------------------------------------------------------------

  , mkOption "log-level" (Just 'l')
      "Minimum urgency that gets displayed: error | warn | info | debug"
      $ setWithExpr logLevel logLevelExpr "logLevel"

  , mkOption "log-color" (Just 'C')
      "Coloration strategy for logging output: dark-bg | light-bg | none"
      $ setWithExpr logColor logColorExpr "logColor"

    -- KioCfg ------------------------------------------------------------------

  , mkOption "key-input" (Just 'i')
      "Input expression defining how to acquire keyboard"
      $ setWithExpr keyInputCfg keyInputExpr "keyInputCfg"

  , mkOption "key-output" (Just 'o')
      "Output expression defining how to simulate keyboard"
      $ setWithExpr keyOutputCfg keyOutputExpr "keyOutputCfg"

  , mkOption "pre-kio-cmd" (Just 'b')
      "Command expression to be run before acquiring key-IO"
      $ setWithExpr preKioCmd cmdExpr "preKioCmd"

  , mkOption "post-kio-cmd" (Just 'a')
      "Command expression to be run before acquiring key-IO"
      $ setWithExpr postKioCmd cmdExpr "postKioCmd"

  , mkOption "key-repeat" (Just 'e')
      "KeyRepeat expression describing how to handle key-repeat"
      $ setWithExpr keyRepeatCfg keyRepeatExpr "keyRepeatCfg"
  ]
