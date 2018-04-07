module DB (loadState, loadRecords, writeState, updateRecord) where
import Types
import Data.List
import System.Directory

data Record = Record { state :: String, partnerId :: String, userId :: String }
    deriving (Show, Read)

parseState :: String -> UserState
parseState "NonExistent" = NonExistent
parseState "Inactive" = Inactive
parseState "Pending" = Pending
parseState "Active" = Active
parseState "Suspended" = Suspended

toState :: Record -> UserState
toState = parseState . state

toStateString :: UserState -> String
toStateString Active = "Active"
toStateString Inactive = "Inactive"
toStateString Pending = "Pending"
toStateString NonExistent = "NonExistent"
toStateString Suspended = "Suspended"

loadRecords :: () -> IO ([DB.Record])
loadRecords _ = do 
        txt <- readFile "store.txt"
        let records = read txt :: [Record]
        return records

loadRecord :: RecordIdentifier -> IO (Maybe Record)
loadRecord id = do
    records <- loadRecords ()
    return (findRecord id records)

findRecord :: RecordIdentifier -> [Record] -> Maybe Record
findRecord (UserId ui)    = find (\r -> userId r    == ui) 
findRecord (PartnerId pi) = find (\r -> partnerId r == pi) 

loadState :: RecordIdentifier -> IO (Maybe UserState)
loadState id = do
    maybeRecord <- loadRecord id
    return (fmap toState maybeRecord)

saveRecords :: [DB.Record] -> IO ()
saveRecords rs = do
    writeFile "store.tmp" (show rs)
    renameFile "store.tmp" "store.txt"
    

updateRecord :: [Record] -> RecordIdentifier -> UserState -> [Record]
updateRecord [] _ _ = []
updateRecord all@(r@(Record { partnerId = rpid }):rest) (PartnerId pid) state 
    | rpid == pid = (r { state = toStateString state }) : rest 
updateRecord all@(r@(Record{ userId = ruid }):rest) (UserId uid) state 
    | ruid == uid = (r { state = toStateString state }) : rest 
updateRecord (r:rest) id state = r : (updateRecord rest id state)

updateRecords :: ([Record] -> [Record]) -> () -> IO ()
updateRecords f _ = do
    records <- loadRecords ()
    let updatedRecords = f records
    saveRecords updatedRecords

writeState :: RecordIdentifier -> UserState -> IO ()
writeState id state = updateRecords (\records -> updateRecord records id state) ()

addState :: RecordIdentifier -> UserState -> IO ()
addState id state =
    updateRecords (\records -> record:records) ()
        where
            record = case id of
                UserId uid    -> Record { state = "", partnerId = "", userId = uid }
                PartnerId pid -> Record { state = "", partnerId = pid, userId = "" }


