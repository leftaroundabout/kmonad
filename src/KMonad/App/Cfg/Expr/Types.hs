-- |

module KMonad.App.Cfg.Expr.Types where

import KMonad.Prelude
import KMonad.Prelude.Parsing

import KMonad.App.Cfg.Types

-- expr ------------------------------------------------------------------------

-- | An 'Expr' describing the relationship between 'Text' and some value
data Expr a = Expr
  { _toText :: a -> Text                          -- ^ How to encode a value to text
  , _fromText :: Text -> Either ParseError a      -- ^ How to decode a value from text
  , _errPrism :: AReview SomeException ParseError -- ^ How to render a decoding error
  }
makeLenses ''Expr

exprIso :: Expr a -> Iso' a Text
exprIso x = iso (x^.toText) $ \t -> throwEither (x^.errPrism) (x^.fromText $ t)

-- named-expr types ------------------------------------------------------------

newtype NamedExprError = NamedParseError ParseError deriving Eq
makeClassyPrisms ''NamedExprError

instance Show NamedExprError where
  show (NamedParseError e) =
    "Parse error in Named expression:\n" <> show e

instance Exception NamedExprError
instance AsNamedExprError SomeException where _NamedExprError = exception

namedExpr :: Eq a => Named a -> Expr a
namedExpr ns = Expr nameFor' (parse $ namedP ns) _NamedParseError
  where
    nameFor' a = fromMaybe (error msg) $ nameFor a ns
    msg = "Programmer did not specify names for every option. Please complain on github."

-- named-expr vals -------------------------------------------------------------

boolExpr :: Expr Bool
boolExpr = namedExpr [("off", False), ("on", True)]

runTypeExpr :: Expr RunType
runTypeExpr = namedExpr [("run", FullRun), ("test", CfgTest), ("discover", EvTest)]

logLevelExpr :: Expr LogLevel
logLevelExpr = namedExpr [ ("error", LevelError), ("warn", LevelWarn)
                         , ("info", LevelInfo), ("debug", LevelDebug) ]

cmdAllowExpr :: Expr CmdAllow
cmdAllowExpr = namedExpr [("none", NoCmds), ("init", InitCmds), ("all", AllCmds)]
