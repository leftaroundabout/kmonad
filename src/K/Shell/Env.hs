-- |

module K.Shell.Env
  ( withAppEnv
  , AppEnv(..)
  )
where

import K.Shell.Env.Initial

withAppEnv :: AppCfg -> (AppEnv -> m a) -> m a
withAppEnv _ f = f AppEnv
