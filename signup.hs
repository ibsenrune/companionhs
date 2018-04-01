import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import Types
import DB

data Command = Command UserState
data Input = Input RecordIdentifier UserState deriving (Show, Read)

transition :: UserState -> Command -> Maybe UserState
transition NonExistent (Command Inactive)  = Just Inactive
transition Inactive    (Command Active)    = Just Active
transition Inactive    (Command Pending)   = Just Pending
transition Pending     (Command Active)    = Just Active
transition Active      (Command Suspended) = Just Suspended
transition _           (Command _)         = Nothing

inputLoop :: IO ()
inputLoop = do
    putStrLn "Tell me what to do:"
    command <- getLine
    when (command == ":q") $ return ()
    let Input id targetState = read command :: Input
    state <- loadState id
    when (state & isNothing) $ return ()
    let cmd = Command targetState
    let startState = fromJust state
    let endState = transition startState cmd
    if (endState & isNothing)
      then do
          putStrLn "Cannot transition like that"
      else do
            putStrLn ("Action: " ++ (show (Input id targetState)))
    inputLoop







