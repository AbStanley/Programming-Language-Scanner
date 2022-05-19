module Qualifier where

    import Data.Char
 
    isBool :: String -> Bool
    isBool b = elem b ["True","False"]

    isInt :: String -> Bool
    isInt n = not $ elem False (map isDigit n)

    isDouble :: String -> Bool
    isDouble n
        | head n == '.' = False
        | tail n == "." = False
        | otherwise     = True

    isString :: Char -> Bool
    isString c = c == '\"'

    isChar :: Char -> Bool
    isChar c = c == '\''

    isKeyword :: String -> Bool
    isKeyword s = elem s ["if","else","let","while"]

    isSingleOperator :: Char -> Bool
    isSingleOperator c = elem c "+-*/!<>"

    isDoubleOperator :: String -> Bool
    isDoubleOperator s = elem s ["==","!=",">=","<=","&&","||"]

    isDelimiter :: Char -> Bool
    isDelimiter c = elem c "=,{[(}]);"

    isNum :: Char -> Bool
    isNum n = elem n "1234567890."

    isWord :: Char -> Bool
    isWord c = isAlpha c

    isIdentifier :: Char -> Bool
    isIdentifier i = (isAlphaNum i) || (i == '_')

    isOperator :: String -> Bool
    isOperator n = (isDelimiter c) || (isSingleOperator c) || (isDoubleOperator x)
      where c = head n
            x = take 2 n

    isQuote :: String -> Bool
    isQuote (x:y:_) = (x == '\\') && (y == '\"')

    isNewLine :: String -> Bool
    isNewLine (x:y:_) = (x == '\\') && (y == 'n')

    isBackslash :: String -> Bool
    isBackslash (x:_) = (x == '\\')

    isDoubleBackslash :: String -> Bool
    isDoubleBackslash (x:y:_) = (x == '\\') && (y == '\\')

    isNotQuote :: String -> Bool
    isNotQuote (x:_) = not (isString x)

    isInvalid :: String -> Bool
    isInvalid xs = elem '\n' (init xs)

    isEndLine :: String -> Bool
    isEndLine (x:xs) = (x == '\n')