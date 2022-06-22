-- |

module K.Shell
  ( begin
  , IO
  )
where

import K.Initial
import K.Shell.Cfg
import K.Shell.Error
import K.Shell.Logging

import qualified Control.Exception.Lens as Exc


begin :: IO ()
begin = do
  initLogging defAppCfg . inRIO $ do

    -- Final exception-catching mechanism in KMonad
    let handle err = do
          Exc.throwing _AppError err

    Exc.handling _AppError handle $ do
      ivk <- getInvoc

      logInfo "Hello sailor!"
      Exc.throwing _Monkeys ()
