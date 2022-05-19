module Scanner (tokens) where

    import Data.Char
    import Data.List
    import Token
    import Qualifier
    import TokenAssigner
    
    tokens :: String -> [Token]
    tokens x = (token_scanner x)

    token_scanner :: String -> [Token]
    token_scanner [] = [EofToken]
    token_scanner cs@(c: cs')
        | isChar c            = scan_character cs
        | isString c          = scan_string cs
        | isNum c             = scan_number c cs'
        | isWord c            = scan_word c cs'
        | isOperator cs       = scan_operator cs
        | isSpace c           = token_scanner cs'
        | otherwise = ErrorToken UnknownSymbol : token_scanner cs'

    scan_character :: String -> [Token]
    scan_character (_:[]) = ErrorToken InvalidCharacter : EofToken : token_scanner []
    scan_character (_:x:y:z:xs)
        | (x == '\\') && (y == 'n')  = ErrorToken InvalidCharacter : token_scanner xs'
        | (x == '\\') && (z == '\'') = CharToken y : token_scanner xs
        | y == '\''                  = CharToken x : token_scanner xs'
        | otherwise = ErrorToken InvalidCharacter : token_scanner xs'
        where xs' = z:xs

    scan_string :: String -> [Token]
    scan_string (_:[]) = StringToken "" : EofToken : token_scanner []
    scan_string (_:s)
        | xs /= [] = StringToken xs : token_scanner ys'
        | xs == [] = StringToken "" : token_scanner ys'
        where 
          (xs, ys) = scan_special_characters s
          ys' = drop 1 ys
          ys'' = scan_newline s

    scan_number :: Char -> String -> [Token]
    scan_number c [] = scan_a_number [c] : EofToken : token_scanner []
    scan_number c cs = scan_a_number (c : digs) : token_scanner cs'
        where (digs, cs') = span isNum cs

    scan_word :: Char -> String -> [Token]
    scan_word c [] = scan_a_word [c] : EofToken : token_scanner []
    scan_word c cs = scan_a_word (c:str) : token_scanner cs'
        where (str, cs') = span isIdentifier cs

    scan_operator :: String -> [Token]
    scan_operator (x:[])
        | isSingleOperator x  = OperatorToken (singleOperator x) : EofToken : token_scanner []
        | isDelimiter x       = DelimiterToken (delimiter x) : EofToken : token_scanner []
    scan_operator xs
        | isDoubleOperator y = OperatorToken (doubleOperator y) : token_scanner xs''
        | isSingleOperator x  = OperatorToken (singleOperator x) : token_scanner xs'
        | isDelimiter x       = DelimiterToken (delimiter x) : token_scanner xs'
        where 
            x     = head xs
            xs'   = drop 1 xs
            y     = take 2 xs
            xs''  = drop 2 xs

    scan_newline :: String -> String
    scan_newline [] = []
    scan_newline ss@(x:xs@(y:xs'))
        | not ((x == '\\') && (y == 'n')) = scan_newline xs
        | otherwise = ss

    scan_special_characters :: String -> (String, String)
    scan_special_characters xs@(x:xs')
        | isQuote xs       = ('\"':ys', zs')
        | isNewLine xs     = ('\n':ys', zs')
        | isDoubleBackslash xs = ('\\':ys', zs')
        | isBackslash xs   = (ys, zs)
        | isEndLine xs = ((read '\n'::Char): ys, zs)
        | isNotQuote xs    = (x:ys, zs)
        | otherwise = ([],xs)
        where 
            xs'' = drop 1 xs'
            (ys, zs)   = scan_special_characters xs'
            (ys', zs') = scan_special_characters xs''

    scan_a_word :: String -> Token
    scan_a_word x
        | isKeyword x  = KeywordToken (keyword x)
        | isBool x     = BoolToken (boolean x)
        | otherwise    = IdentifierToken x

    scan_a_number :: String -> Token
    scan_a_number x
        | isInt x    = IntToken (read x)
        | isDouble x = DoubleToken (read x)
        | otherwise  = ErrorToken InvalidDouble
