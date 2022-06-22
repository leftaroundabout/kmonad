-- |

module K.Shell.Logging where

import K.Shell.Initial
import K.Shell.Cfg (HasLogCfg(..))


-- basic types -----------------------------------------------------------------


data LogEnv = LogEnv
  { _logfun :: LogFunc }
makeClassy ''LogEnv

instance HasLogFunc LogEnv where logFuncL = logfun




-- ops -------------------------------------------------------------------------

initLogging :: (MonadUnliftIO m, HasLogCfg cfg) => cfg -> (LogEnv -> m a) -> m a
initLogging c f = do
  o <- logOptionsHandle stdout False <&> setLogMinLevel (c^.logLevel)
  withLogFunc o $ f . LogEnv
