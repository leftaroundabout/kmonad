-- |

module K.Shell.Env
  ( withAppEnv
  , AppEnv(..)
  )
where

import K.Shell.Env.Initial

{- NOTE: I am really not sure if this is a good way of doing things.

Most of the code is written in MTL-style, so they only have access to what they
need. However, getting all the environments together in the right order proved
complicated. For example, KeyIO depends on logging, and KeyIO is also 3
different environments. All these environments need to be nested in brackets.

The withAppEnv function works only because we're very careful to never use
anything that isn't defined yet. Which is questionable. However, other solutions
I tried just don't compose pleasantly, and it becomes a huge headache. So, I've
written nearly all the code in clear, MTL style, and moved *all* of my warts
into this function below.

-}

-- | Initialize an 'AppEnv' from an 'AppCfg' and call a function on it.
withAppEnv :: UIO m => AppCfg -> (AppEnv -> m a) -> m a
withAppEnv c f = do
  u <- askRunInIO
  let null = AppEnv { _appAppCfg = undefined
                    , _appLogEnv = undefined
                    , _appKioEnv = undefined
                    , _appRunEnv = undefined }
  runRIO null $ do
    withLogging c $ \le -> locally logEnv (const le) $ do -- uses nothing
      withKio c $ \ke -> locally kioEnv (const ke) $ do   -- uses logenv
        RIO . ReaderT $ u . f
