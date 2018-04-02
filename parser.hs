module Parser where
{- Command parser 
 - UserId(abc) -> Active
 - PartnerId(123) -> Inactive
 - -}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Monad (void)
import Types

data Command = Command RecordIdentifier UserState
    deriving (Show, Read)

cmd :: Parser Command
cmd = do
    id <- identifier
    mapTo
    s <- state
    return $ Command id s

identifier :: Parser RecordIdentifier
identifier = userId <|> partnerId

partnerId :: Parser RecordIdentifier
partnerId = ident "PartnerId" PartnerId

userId :: Parser RecordIdentifier
userId = ident "UserId" UserId

ident :: String -> (String -> RecordIdentifier) -> Parser RecordIdentifier
ident typId ctor = do
    string typId
    id <- between (char '(') (char ')') (many alphaNum)
    return $ ctor id

mapTo :: Parser String
mapTo = string "->"

state :: Parser UserState
state = do
    s <- 
        string "nonexistent" <|>
        string "inactive" <|>
        string "pending" <|>
        string "active" <|>
        string "suspended"
    return $ case s of
        "nonexistent" -> NonExistent
        "inactive" -> Inactive
        "pending" -> Pending
        "active" -> Active
        "suspended" -> Suspended

ws :: Parser ()
ws = void $ many $ oneOf "\n\t"


