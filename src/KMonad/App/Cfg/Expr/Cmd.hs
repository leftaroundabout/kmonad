-- |

module KMonad.App.Cfg.Expr.Cmd where


import KMonad.Prelude
import KMonad.Prelude.Parsing

-- basic types -----------------------------------------------------------------

data Cmd
  = SimpleCmd { _fullcmd :: Text}
  | CompoundCmd { _executable :: FilePath
                , _args :: [Text] }
    deriving (Eq, Show)

type CmdExpr = Text

newtype CmdExprError = CmdParseError ParseError deriving Eq
makeClassyPrisms ''CmdExprError

instance Show CmdExprError where
  show (CmdParseError e) =
    "Parse error in Cmd expression:\n" <> show e

instance Exception CmdExprError
instance AsCmdExprError SomeException where _CmdExprError = exception

-- basic ops -------------------------------------------------------------------

-- | An Iso between 'Text' and 'Cmd' values
_CmdExpr :: Iso' Cmd CmdExpr
_CmdExpr = iso cmdT $ throwEither _CmdParseError . parse cmdP

-- exprs -----------------------------------------------------------------------

{- NOTE: The basic syntax for cmds is:
- a preamble containing either 'exec:' or 'cmd:'
- if 'cmd:' -> The rest of the string is a shell command
- if 'exec:' -> 'name-of-command':, then list of command-separated string args

"cmd:echo foo > /tmp/example.txt"
"exec:rm:["/tmp/*", "-r", "-f"]" (in haskell, you'd have to escape the "-marks)

no shorthand at the moment

-}

-- | Create the textual representation of a 'Cmd'
cmdT :: Cmd -> CmdExpr
cmdT (SimpleCmd t) = "cmd:" <> t
cmdT (CompoundCmd e a) = "exec:" <> pack e <> ":" <> tshow a

-- | A parser that tries to extract a 'Cmd' from 'Text'
--
-- Patterns
-- - "cmd:evtest this thing for me"
-- - "exec:rm:["/*", "-rf"]"
cmdP :: Parser Cmd
cmdP = choice
  [ string "cmd:" *> (SimpleCmd <$> takeRest)
  , string "exec:" *> (CompoundCmd <$> exe <*> args)
  ]
  where
    exe = unpack <$> takeWhile1P Nothing (/= ':') <* char ':'
    args = listOfP textP
