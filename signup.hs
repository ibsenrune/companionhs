import Data.List

data Record = Record { state :: String, partnerId :: String, userId :: String }
    deriving (Show, Read)
data RecordIdentifier = PartnerId String | UserId String

data UserState =
    NonExistent
    | Inactive
    | Pending
    | Active
    | Suspended
    deriving (Show, Eq)

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

saveRecords :: [Record] -> IO ()
saveRecords rs =
    do writeFile "store.txt" (show rs)

findRecord :: RecordIdentifier -> [Record] -> Maybe Record
findRecord (PartnerId pi) = find (\r -> partnerId r == pi) 
findRecord (UserId ui) = find (\r -> userId r == ui) 

loadRecord :: RecordIdentifier -> IO (Maybe Record)
loadRecord id =
    do txt <- readFile "store.txt"
       let records = read txt :: [Record]
       return (findRecord id records)
