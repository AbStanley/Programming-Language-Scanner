module TokenAssigner where

    import Token

    boolean :: String -> Bool
    boolean b
        | b == "True"  = True
        | b == "False" = False

    keyword :: String -> Keyword
    keyword s
        | s == "if"    = If
        | s == "else"  = Else
        | s == "let"   = Let
        | s == "while" = While

    doubleOperator :: String -> Operator
    doubleOperator s
        | s == "==" = Equal
        | s == "!=" = NotEqual
        | s == ">=" = GreaterEq
        | s == "<=" = LessEq
        | s == "&&" = And
        | s == "||" = Or

    singleOperator :: Char -> Operator
    singleOperator c
        | c == '+' = Plus
        | c == '-' = Minus
        | c == '*' = Times
        | c == '/' = Divide
        | c == '<' = Less
        | c == '>' = Greater
        | c == '!' = Not

    delimiter :: Char-> Delimiter
    delimiter c
        | c == '=' = Assign
        | c == ',' = Comma
        | c == '{' = LBrace
        | c == '[' = LBracket
        | c == '(' = LParen
        | c == '}' = RBrace
        | c == ']' = RBracket
        | c == ')' = RParen
        | c == ';' = Semicolon
