-- |

module Error where

import Control.Monad.Except

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- define show for LispError
showError :: LispError -> String
showError (UnboundVar msg varName) = msg ++ ": " ++ varName
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg fn) = msg ++ ": " ++ show fn
showError (NumArgs exptd fnd) = "Expected " ++ show exptd ++ " args, but found "
  ++ unwordsList fnd
showError (TypeMismatch exptd fnd) = "Invalid type: expected " ++ exptd ++ " but found " ++ show fnd
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
