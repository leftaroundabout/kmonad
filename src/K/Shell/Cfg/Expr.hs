-- |

module K.Shell.Cfg.Expr
  ( -- * Basic types and operations
    Expr
  , ExprError
  , AsExprError(..)
  , decode
  , encode

    -- * Simple expressions
  , boolExpr
  , runTypeExpr
  , logLevelExpr
  , cmdAllowExpr

    -- * Complex expressions
    -- ** Path
  , module K.Shell.Cfg.Expr.Path
    -- ** Cmd
  , module K.Shell.Cfg.Expr.Cmd
    -- ** KeyIO
  , module K.Shell.Cfg.Expr.KeyInput
  , module K.Shell.Cfg.Expr.KeyOutput
  , module K.Shell.Cfg.Expr.KeyRepeat
    -- ** Toggle sequences
  , module K.Shell.Cfg.Expr.Toggles

  )
where

import K.Shell.Cfg.Expr.Initial
import K.Shell.Cfg.Expr.Cmd
import K.Shell.Cfg.Expr.KeyInput
import K.Shell.Cfg.Expr.KeyOutput
import K.Shell.Cfg.Expr.KeyRepeat
import K.Shell.Cfg.Expr.Path
import K.Shell.Cfg.Expr.Toggles

-- simple expressions ----------------------------------------------------------

-- | An expression binding @on@ and @off@ to 'True' and 'False' respectively
boolExpr :: Expr Bool
boolExpr = namedExpr "Bool"
  [ ("off", False)
  , ("on", True)
  ]

-- | An expression binding names to the 'RunType' values
runTypeExpr :: Expr RunType
runTypeExpr = namedExpr "RunType"
  [ ("run", FullRun)
  , ("test", CfgTest)
  , ("discover", EvTest)
  ]

-- | An expression binding names to the 4 'LogLevel' values
logLevelExpr :: Expr LogLevel
logLevelExpr = namedExpr "LogLevel"
  [ ("error", LevelError)
  , ("warn", LevelWarn)
  , ("info", LevelInfo)
  , ("debug", LevelDebug)
  ]

-- | An expression binding names to the different 'CmdAllow' values
cmdAllowExpr ::  Expr CmdAllow
cmdAllowExpr = namedExpr "CmdAllow"
  [ ("none", NoCmds)
  , ("init", InitCmds)
  , ("all", AllCmds)
  ]
