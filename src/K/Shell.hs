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
  initLogging defAppCfg $ inRIO $ do

    let handle err = do
          logError "Encountered unhandled, top-level error. Exiting:\n"
          throwing err

    Exc.handling _AppError (\_ -> pure ()) $ do
      logInfo "Hello sailor!"
