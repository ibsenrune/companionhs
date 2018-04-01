module Types where

data RecordIdentifier = PartnerId String | UserId String
    deriving (Show, Read)

data UserState =
    NonExistent
    | Inactive
    | Pending
    | Active
    | Suspended
    deriving (Show, Eq, Read)


