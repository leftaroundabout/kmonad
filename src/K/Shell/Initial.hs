-- |

module K.Shell.Initial
  ( RunType(..)
  , CmdAllow(..)
  , DelayRate(..)

  , module K.Initial
  , module K.Gesture
  , module K.Keyboard
  )

where

import K.Initial
import K.Gesture
import K.Keyboard

-- basic types -----------------------------------------------------------------

-- | The task to execute during this kmonad run.
data RunType
  = FullRun -- ^ Do a normal load-cfg-and-remap-my-keyboard run
  | CfgTest -- ^ Only try parsing all the configuration, then exit
  | EvTest  -- ^ Parse cfg, then run a read-print loop on the input keyboard
  deriving (Eq, Show)

-- | Different allowances for running shell-commands from kmonad
data CmdAllow
  = AllCmds  -- ^ Allow all commands
  | InitCmds -- ^ Allow only the commands involved in KeyIO initialization
  | NoCmds   -- ^ Allow no commands
  deriving (Eq, Show)

-- | Description of key-repeat behavior
data DelayRate = DelayRate
  { _delay :: Dt -- ^ How long to wait before repeating starts
  , _rate  :: Dt -- ^ How long between each repeat event
  } deriving (Eq, Show)
