{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Database.Flat.JSON where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           Data.Text(Text)
import           Data.Time.Clock

------------------------------------------------------------------------------------
-- | A CRUD is a OO-style database Table of typed rows, with getters and setters.

class CRUD f where
  createRow :: Object       -> f Row
  readRow   :: Id           -> f (Maybe Row)
  updateRow :: Id -> Object -> f () -- The _id and _ts fields are ignored in the object.
  deleteRow :: Id           -> f ()

------------------------------------------------------------------------------------
-- Basic synonyms for key structures
--
-- | Every (Named) Row must have an id field.
type Id        = Text

------------------------------------------------------------------------------------
-- | A Row is a Object, with "_id" (the unique identifier, a String),
--   and a "_ts" (the timestamp), in 'UTCTime' format, as meta-data.
--   This "_ts" field should be a record if *when* it was written.

newtype Row = Row Object
   deriving (Eq,Show)

instance FromJSON Row where
    parseJSON (Object v) = 
         pure Row <* (v .: "_id" :: Parser Text)
                  <* (v .: "_ts" :: Parser UTCTime)
                  <*> pure v
    parseJSON _ = fail "row should be an object"

instance ToJSON Row where
   toJSON (Row o) = Object o

rowId :: Row -> Text
rowId (Row o) = case parse (.: "_id") o of
                  Success v -> v
                  Error msg -> error $ "rowId failed " ++ msg

rowUTCTime :: Row -> Maybe UTCTime
rowUTCTime (Row o) = case parse (.: "_ts") o of
                  Success v -> Just v
                  Error _ -> Nothing

lookupRow :: FromJSON a => Text -> Row -> Result a
lookupRow field (Row o) = parse (.: field) o 

----------------------------------------------------------

data TableUpdate
        = RowUpdate Row       -- update a row with a new row, by _id.
        | RowDelete Id        -- consider previous updates to row deleted
        | Shutdown Text       -- last message; please stop listening. Msg for informational purposes ony.
        deriving (Show, Eq)
        
instance ToJSON TableUpdate where
   toJSON (RowUpdate row)      = toJSON row
   toJSON (RowDelete key)      = object ["delete"   .= key]     -- no _id key
   toJSON (Shutdown msg)       = object ["shutdown" .= msg]     -- no _id key

instance FromJSON TableUpdate where
    parseJSON (Object v) = 
         RowUpdate <$> parseJSON (Object v) <|> 
         RowDelete <$> v .: "delete"        <|>
         Shutdown  <$> v .: "shutdown"
    parseJSON _ = error "TableUpdate Object was not a valid Object"


tableUpdate :: TableUpdate -> HashMap Text Row -> HashMap Text Row
tableUpdate (RowUpdate row) = HashMap.insert (rowId row) row
tableUpdate (RowDelete key) = HashMap.delete key
tableUpdate (Shutdown _msg) = id
