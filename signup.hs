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
data Input = Input RecordIdentifier UserState deriving (Show, Read)

toState :: Record -> UserState
toState Record { state = "NonExistent" } = NonExistent
toState Record { state = "Inactive" } = Inactive
toState Record { state = "Pending" } = Pending
toState Record { state = "Active" } = Active
toState Record { state = "Suspended" } = Suspended

toStateString :: UserState -> String
toStateString Active = "Active"
toStateString Inactive = "Inactive"
toStateString Pending = "Pending"
toStateString NonExistent = "NonExistent"
toStateString Suspended = "Suspended"

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

loadRecords :: () -> IO ([Record])
loadRecords _ = do 
        txt <- readFile "store.txt"
        let records = read txt :: [Record]
        return records

loadRecord :: RecordIdentifier -> IO (Maybe Record)
loadRecord id = do
    records <- loadRecords ()
    return (findRecord id records)

saveRecords :: [Record] -> IO ()
saveRecords rs =
    do writeFile "store.txt" (show rs)

updateRecord :: [Record] -> RecordIdentifier -> UserState -> [Record]
updateRecord [] _ _ = []
updateRecord all@(r@(Record { partnerId = rpid }):rest) (PartnerId pid) state | rpid == pid = 
    (r { state = toStateString state}) : rest 
updateRecord all@(r@(Record{ userId = ruid }):rest) (UserId uid) state | ruid == uid =
    (r { state = toStateString state  }) : rest 
updateRecord (r:rest) id state = r : (updateRecord rest id state)

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







