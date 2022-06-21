-- |

module K.Shell.Cfg.Initial where

import K.Prelude
import K.Shell.Initial
import K.Keyboard.Gesture

import qualified RIO.HashMap as M

-- config adts -----------------------------------------------------------------

-- | Different ways to read keyboard input from the world
data KeyInputCfg
  = LinEvdevSrc Path         -- ^ Linux evdev keyboard
  | WinHookSrc               -- ^ Windows low-level keyboard hook source
  | MacIOKitSrc (Maybe Text) -- ^ Mac IOKit keyboard source
  | CmdSrc Cmd               -- ^ Read stdout from some shell-command
  | StdinSrc                 -- ^ Read directly from stdin
  deriving (Eq, Show)

-- | Different ways to write keyboard output to the world
data KeyOutputCfg
  = LinUinputSnk (Maybe Text) -- ^ Linux uinput keyboard
  | WinSendSnk                -- ^ Windows SendEvent keyboard
  | MacKextSink               -- ^ Mac Kext/Dext keyboard
  | CmdSnk Cmd                -- ^ Write to stdin of some shell-command
  | StdoutSnk                 -- ^ Write directly to stdout
  deriving (Eq, Show)

-- | What to do about key-repeat behavior
data KeyRepeatCfg
  = Simulate DelayRate -- ^ Internally simulate key-repeat process
  | EchoOS             -- ^ Use OS-provided repeat events to trigger key-repeats
  | IgnoreRepeat       -- ^ Don't model key-repeat at all
  deriving (Eq, Show)

-- app cfg records -------------------------------------------------------------

-- | Settings that deal with binding names to keyboard concepts
data LocaleCfg = LocaleCfg
  { _namedCodes :: NameMap Keycode
    -- ^ A collection of name-to-keycode correspondences
  , _namedGestures :: NameMap (Gesture Keycode)
    -- ^ A collection of name-to-gesture correspondences
  } deriving (Eq, Show)

-- | Settings that deal with task and permissions
data RunCfg = RunCfg
  { _cfgPath :: Path      -- ^ Where to look for the .dhall configuration file
  , _kbdPath :: Path      -- ^ Where to look for the .kbd keymap file
  , _cmdAllow :: CmdAllow -- ^ What style of command-execution policy to use
  , _runType :: RunType   -- ^ What task to perform
  } deriving (Eq, Show)

-- | Settings that deal with keyboard IO
data KioCfg = KioCfg
  { _keyRepeatCfg :: KeyRepeatCfg  -- ^ How to handle key-repeat
  , _fallthrough  :: Bool          -- ^ Whether to reemit uncaught events
  , _keyInputCfg  :: KeyInputCfg   -- ^ How to acquire keyboard input
  , _keyOutputCfg :: KeyOutputCfg  -- ^ How to emit keyboard output
  , _preKioCmd    :: Cmd           -- ^ Command to run before initializing KeyIO
  , _postKioCmd   :: Cmd           -- ^ Command to run after initializing KeyIO
  } deriving (Eq, Show)

-- | Settings that deal with what and how to log information
newtype LogCfg = LogCfg
  { _logLevel :: LogLevel -- ^ Minimum urgency for messages to be displayed
  } deriving (Eq, Show)

-- | Collection of all configurations put together.
data AppCfg = AppCfg
  { _appLocaleCfg :: LocaleCfg
  , _appLogCfg :: LogCfg
  , _appKioCfg :: KioCfg
  , _appRunCfg :: RunCfg
  } deriving (Eq, Show)

-- lenses ----------------------------------------------------------------------

makeClassy ''RunCfg
makeClassy ''LocaleCfg
makeClassy ''KioCfg
makeClassy ''LogCfg
makeClassy ''AppCfg

instance HasLogCfg    AppCfg where logCfg    = appLogCfg
instance HasKioCfg    AppCfg where kioCfg    = appKioCfg
instance HasRunCfg    AppCfg where runCfg    = appRunCfg
instance HasLocaleCfg AppCfg where localeCfg = appLocaleCfg

-- default ---------------------------------------------------------------------

-- | The default 'AppCfg'
defAppCfg :: AppCfg
defAppCfg = AppCfg
  { _appLocaleCfg = LocaleCfg
    { _namedCodes    = M.empty
    , _namedGestures = M.empty
    }
  , _appLogCfg = LogCfg
    { _logLevel = LevelWarn
    }
  , _appKioCfg = KioCfg
    { _keyRepeatCfg = IgnoreRepeat
    , _fallthrough = False
    , _keyInputCfg = StdinSrc
    , _keyOutputCfg = StdoutSnk
    , _preKioCmd = PassCmd
    , _postKioCmd = PassCmd
    }
  , _appRunCfg = RunCfg
    { _cfgPath = Path "cfg.dhall" (Just $ XdgCfg "kmonad") False
    , _kbdPath = Path "keymap.kbd" (Just $ XdgCfg "kmonad") False
    , _cmdAllow = NoCmds
    , _runType = FullRun
    }
  }
