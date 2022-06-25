-- |

module K.Shell
  ( begin
  , IO
  )
where

import K.Initial
import K.Shell.Cfg
import K.Shell.Env
import K.Shell.Error
import K.Shell.Logging
import K.Shell.KeyIO

import qualified Control.Exception.Lens as Exc

begin :: IO ()
begin = handleAppError $ do
  -- Get the invocation and apply its changes to the default config
  ivk <- getInvoc
  let ivkCfg = runChange (ivk^.cfgChange) defAppCfg

  -- Load the dhall configuration file
  cfg <- loadCfgFile (ivkCfg^.cfgPath)
  let chg = case validateCfgFile cfg :: Either CfgFileError (Change AppCfg) of
        Left e -> Exc.throwing _CfgFileError e
        Right x -> x

  -- Apply the invoc after applying the cfgfile to the default config
  let cfgCfg = runChange (chg <> ivk^.cfgChange) defAppCfg

  -- Load the klang kbd-file
  -- kbd <- runReaderT loadKbdFile cfgCfg -- (cfgCfg^.kbdPath)

  -- Initialize the AppEnv and run the loop inside
  withAppEnv cfgCfg $ inRIO loop

loop :: RIO AppEnv ()
loop = do
  -- Print all the key events forever
  forever $ do
    e <- waitEvent
    atError $ pp e
    sendEvent e

  -- celebrate
  logInfo "Hello sailor!"
  Exc.throwing _Monkeys ()



  -- pPrint $ ivk^.cfgChange.notes
  -- pPrint $ runChange (ivk^.cfgChange) defAppCfg

  -- logInfo "Hello sailor!"
