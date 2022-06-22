-- |

module K.Shell.Cfg.Expr where

import K.Shell.Cfg.Expr.Initial

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
