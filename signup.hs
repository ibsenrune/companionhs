import Data.List
import Data.Maybe
import Data.Function
import Control.Monad

data Record = Record { state :: String, partnerId :: String, userId :: String }
    deriving (Show, Read)
data RecordIdentifier = PartnerId String | UserId String
    deriving (Show, Read)

data UserState =
    NonExistent
    | Inactive
    | Pending
    | Active
    | Suspended
    deriving (Show, Eq, Read)

data Command = Command UserState

toState :: Record -> UserState
toState Record { state = "NonExistent" } = NonExistent
toState Record { state = "Inactive" } = Inactive
toState Record { state = "Pending" } = Pending
toState Record { state = "Active" } = Active
toState Record { state = "Suspended" } = Suspended

transition :: UserState -> Command -> Maybe UserState
transition NonExistent (Command Inactive)  = Just Inactive
transition Inactive    (Command Active)    = Just Active
transition Inactive    (Command Pending)   = Just Pending
transition Pending     (Command Active)    = Just Active
transition Active      (Command Suspended) = Just Suspended
transition _           (Command _)         = Nothing

findRecord :: RecordIdentifier -> [Record] -> Maybe Record
findRecord (PartnerId pi) = find (\r -> partnerId r == pi) 
findRecord (UserId ui) = find (\r -> userId r == ui) 

loadRecord :: RecordIdentifier -> IO (Maybe Record)
loadRecord id =
    do txt <- readFile "store.txt"
       let records = read txt :: [Record]
       return (findRecord id records)

saveRecords :: [Record] -> IO ()
saveRecords rs =
    do writeFile "store.txt" (show rs)

data Input = Input RecordIdentifier UserState deriving (Show, Read)

inputLoop :: IO ()
inputLoop = do
    putStrLn "Tell me what to do:"
    command <- getLine
    when (command == ":q") $ return ()
    let Input id targetState = read command :: Input
    record <- loadRecord id
    when (record & isNothing) $ return ()
    let cmd = Command targetState
    let r = fromJust record
    let startState = toState r
    let endState = transition startState cmd
    if (endState & isNothing)
      then do
          putStrLn "Cannot transition like that"
      else do
            putStrLn ("Action: " ++ (show (Input id targetState)))
    inputLoop







