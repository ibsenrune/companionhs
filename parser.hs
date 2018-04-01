module Parser where
{- Command parser 
 - UserId(abc) -> Active
 - PartnerId(123) -> Inactive
 - -}

import Text.ParserCombinators.Parsec
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

userId :: Parser RecordIdentifier
userId = do 
    string "UserId"
    char '('
    id <- manyTill anyChar (char ')')
    return $ UserId id

partnerId :: Parser RecordIdentifier
partnerId = do
    string "PartnerId"
    char '('
    id <- manyTill anyChar (char ')')
    return $ PartnerId id

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


