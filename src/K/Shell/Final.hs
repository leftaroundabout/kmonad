-- |

module K.Shell.Final where

begin :: IO ()
begin = do
  initLogging defAppCfg $ inRIO $ do

    let handle err = do
          logError "Encountered unhandled, top-level error. Exiting:\n"
          throwing err

    Exc.handling _AppError (\_ -> pure ()) $ do
      logInfo "Hello sailor!"
