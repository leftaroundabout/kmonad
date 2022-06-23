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

import qualified Control.Exception.Lens as Exc

begin :: IO ()
begin = handleAppError $ do
  -- Get the invocation and apply its changes to the default config
  ivk <- getInvoc
  let ivkCfg = runChange (ivk^.cfgChange) defAppCfg

  -- Load the dhall configuration file and apply its changes to the ivkCfg
  cfg <- loadCfgFile (ivkCfg^.cfgPath)
  let chg = case validateCfgFile cfg :: Either CfgFileError (Change AppCfg) of
        Left e -> Exc.throwing _CfgFileError e
        Right x -> x
  let cfgCfg = runChange chg ivkCfg

  -- Load the klang kbd-file
  kbd <- loadKbdFile (cfgCfg^.kbdPath)

  -- Initialize the AppEnv
  withAppEnv $ \env -> inRIO loop

loop :: RIO AppEnv ()
loop = do
  -- celebrate
  Exc.throwing _Monkeys ()



  -- pPrint $ ivk^.cfgChange.notes
  -- pPrint $ runChange (ivk^.cfgChange) defAppCfg

  -- logInfo "Hello sailor!"
