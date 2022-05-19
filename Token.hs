module Token where

  data Keyword = If
               | Else
               | Let
               | While
       deriving (Show, Eq)

  data Operator = Plus
                | Minus
                | Times
                | Divide
                | Equal
                | NotEqual
                | Less
                | GreaterEq
                | Greater
                | LessEq
                | And
                | Or
                | Not
       deriving (Show, Eq)

  data Delimiter = Assign
                 | Comma
                 | LBrace
                 | LBracket
                 | LParen
                 | RBrace
                 | RBracket
                 | RParen
                 | Semicolon
       deriving (Show, Eq)

  data Error = InvalidDouble
             | InvalidString
             | InvalidCharacter
             | UnknownSymbol
       deriving (Show, Eq)

  data Token = BoolToken Bool
             | IntToken Int
             | DoubleToken Double
             | StringToken String
             | CharToken Char
             | IdentifierToken String
             | KeywordToken Keyword
             | OperatorToken Operator
             | DelimiterToken Delimiter
             | ErrorToken Error
             | EofToken
       deriving (Show, Eq)
