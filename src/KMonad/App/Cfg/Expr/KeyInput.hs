-- |

module KMonad.App.Cfg.Expr.KeyInput where

import KMonad.Prelude

import KMonad.Prelude.Parsing
import KMonad.App.Cfg.Expr.Cmd
import KMonad.App.Cfg.Expr.Path

-- basic types -----------------------------------------------------------------

data KeyInputCfg
  = LinEvdevSrc Path
  | WinHookSrc
  | MacIOKitSrc (Maybe Text)
  | CmdSrc Cmd
  | StdinSrc
  deriving (Eq, Show)

type KeyInputExpr = Text

newtype KeyInputExprError = KeyInputParseError ParseError deriving Eq
makeClassyPrisms ''KeyInputExprError

instance Show KeyInputExprError where
  show (KeyInputParseError e) =
    "Parse error in KeyInput expression:\n" <> show e

instance Exception KeyInputExprError
instance AsKeyInputExprError SomeException where _KeyInputExprError = exception

-- basic ops -------------------------------------------------------------------

_KeyInputExpr :: Iso' KeyInputCfg KeyInputExpr
_KeyInputExpr = iso inputT $ throwEither _KeyInputParseError . parse inputP

_InputPathExpr :: Iso' Path PathExpr
_InputPathExpr = _PathExprWith extraRoots

-- exprs -----------------------------------------------------------------------

extraRoots :: Named PathRoot
extraRoots = [ ("input", Custom "/dev/input/")
             , ("by-id", Custom "/dev/input/by-id/") ]


-- | A parser that tries to extract an 'InputCfg' from 'Text'
--
-- Patterns
-- - lin:evdev:/dev/input8
-- - lin:evdev:glob:/dev/input/by-id/*das*event-kbd
-- - win:hook:
-- - mac:iokit:kb-name
-- - mac:iokit:
-- - cmdsrc:cmd:evtest this thing for me
-- - cmdsrc:exec:evtest:["this", "thing", "for", "me"]
-- - stdin:
inputP :: Parser KeyInputCfg
inputP = choice
  [ string "lin:evdev:" *> (LinEvdevSrc <$> pathP extraRoots)
  , string "win:hook:" $> WinHookSrc
  , string "mac:iokit:" *> (MacIOKitSrc <$> maybeRestP)
  , string "cmdsrc:" *> (CmdSrc <$> cmdP)
  , string "stdin:" $> StdinSrc
  ]

-- | Create the textual representation of an 'KeyInputCfg'
inputT :: KeyInputCfg -> KeyInputExpr
inputT (LinEvdevSrc f) = "lin:evdev:" <> f^._InputPathExpr
inputT WinHookSrc = "win:hook:"
inputT (MacIOKitSrc mt) = "mac:iokit:" <> fromMaybe "" mt
inputT (CmdSrc c) = "cmdsrc:" <> c^._CmdExpr
inputT StdinSrc = "stdin:"
