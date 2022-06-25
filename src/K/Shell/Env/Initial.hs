-- |

module K.Shell.Env.Initial
  ( AppEnv(..)
  , KioEnv(..)
  , RunEnv(..)

  , module K.Shell.Initial
  , module K.Shell.Cfg
  , module K.Shell.Logging
  , module K.Shell.KeyIO
  )
where

import K.Shell.Initial
import K.Shell.Cfg
import K.Shell.Logging
import K.Shell.KeyIO

-- runtime environment ---------------------------------------------------------

data RunEnv = RunEnv

data AppEnv = AppEnv
  { _appAppCfg :: AppCfg
  , _appLogEnv :: LogEnv
  , _appKioEnv :: KioEnv
  , _appRunEnv :: RunEnv
  }
makeClassy ''AppEnv

instance HasLogEnv AppEnv where logEnv = appLogEnv
instance HasKioEnv AppEnv where kioEnv = appKioEnv
instance HasLogFunc AppEnv where logFuncL = logEnv.logFuncL
