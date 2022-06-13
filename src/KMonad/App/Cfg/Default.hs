-- |

module KMonad.App.Cfg.Default where

import KMonad.Prelude

import KMonad.App.Cfg.Types
import KMonad.App.Cfg.Expr

import qualified RIO.HashMap as M


defAppCfg :: AppCfg
defAppCfg = AppCfg
  { _appLocaleCfg = LocaleCfg
    { _namedCodes    = M.fromList []
    , _namedGestures = M.fromList []
    }
  , _appLogCfg = LogCfg
    { _logLevel = LevelWarn
    }
  , _appKioCfg = KioCfg
    { _keyRepeatCfg = IgnoreRepeat
    , _fallthrough = False
    , _keyInputCfg = StdinSrc
    , _keyOutputCfg = StdoutSnk
    , _preKioCmd = NoCmd
    , _postKioCmd = NoCmd
    }
  , _appRunCfg = RunCfg
    { _cfgPath = "xdgcfg:kmonad.dhall" ^. from _PathExpr
    , _kbdPath = "xdgcfg:keymap.kbd" ^. from _PathExpr
    , _cmdAllow = NoCmds
    , _runType = FullRun
    }
  }
