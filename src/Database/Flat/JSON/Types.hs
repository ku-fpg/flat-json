{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Database.Flat.JSON.Types where

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.HashMap.Strict as HashMap

import           Data.Text(Text)
import           Data.Time.Clock

------------------------------------------------------------------------------------
-- | A Crud is a OO-style database Table of typed rows, with getters and setters.

class Crud f where
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
   deriving (Eq, Show)

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

newRow :: Id -> Object -> IO Row
newRow id_ obj = do
        ts_ <- getCurrentTime
        return $ Row $ HashMap.insert "_id" (toJSON id_)
                     $ HashMap.insert "_ts" (toJSON ts_)
                     $ obj 