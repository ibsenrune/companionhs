import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import Types
import DB
import Parser
import Text.ParserCombinators.Parsec

transition :: UserState -> UserState -> Maybe UserState
transition NonExistent (Inactive)  = Just Inactive
transition Inactive    (Active)    = Just Active
transition Inactive    (Pending)   = Just Pending
transition Pending     (Active)    = Just Active
transition Active      (Suspended) = Just Suspended
transition _           (_)         = Nothing

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

getRight :: Either a b -> b 
getRight (Right v) = v

inputLoop :: IO ()
inputLoop = do
    putStrLn "Tell me what to do:"
    command <- getLine
    let parsedCommand = parse cmd "" command 
    when (parsedCommand & isLeft) $ return ()
    let (Command id targetState) = getRight parsedCommand
    putStrLn (show id)
    storedState <- loadState id
    when (storedState & isNothing) $ return ()
    let startState = fromJust storedState
    let endState = transition startState targetState
    if (endState & isNothing)
      then do
          putStrLn "Cannot transition like that"
      else do
            putStrLn ("Action: " ++ (show targetState))
            writeState id targetState
    inputLoop







