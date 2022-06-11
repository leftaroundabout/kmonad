-- |

module KMonad.App.Cfg.Expr.Types where

import KMonad.Prelude
import KMonad.Prelude.Parsing

-- type Expr a = Iso' a Text

data Expr a = Expr
  { _toText :: a -> Text
  , _fromText :: Text -> Either ParseError a
  }
makeLenses ''Expr

exprIso :: Expr a -> AReview SomeException ParseError -> Iso' a Text
exprIso x l = iso (x^.toText) $ \t -> throwEither l (x^.fromText $ t)
  -- Left err -> throwing l err
  -- Right a -> a

data BoolExprError = BoolParseError ParseError deriving Eq
makeClassyPrisms ''BoolExprError

instance Show BoolExprError where
  show (BoolParseError e) =
    "Parse error in Bool expression:\n" <> show e

instance Exception BoolExprError
instance AsBoolExprError SomeException where _BoolExprError = exception
instance AsBoolExprError ParseError where _BoolExprError = _BoolExprError


-- | An expression linking @on@ to 'True' and @off@ to 'False'
boolExpr :: Expr Bool
boolExpr = Expr (bool "off" "on") $ parse boolP
  where boolP = (True <$ string "on") <|> (False <$ string "off")

_Bool :: Iso' Bool Text
_Bool = exprIso boolExpr _BoolParseError
