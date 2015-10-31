{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ConstraintKinds, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Database.Flat.JSON.Types where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)

import           Data.Text(Text)

------------------------------------------------------------------------------------
-- | A CRUD is a OO-style database Table of typed rows, with getters and setters.

type CRUDRow f = (CreateRow f, ReadRow f, UpdateRow f, DeleteRow f)

class CreateRow f where
  createRow :: Object       -> f Row

class ReadRow f where
  readRow   :: Id           -> f (Maybe Row)
  readTable ::                 f Table

class UpdateRow f where
  updateRow :: Id -> Object -> f () -- The _id fields is overwritten in the object.

class DeleteRow f where
  deleteRow :: Id           -> f ()

------------------------------------------------------------------------------------
-- Basic synonyms for key structures
--
-- | Every (Named) Row must have an id field.
type Id        = Text

------------------------------------------------------------------------------------
-- | A Row is a Object, with "_id" (the unique identifier, a String).

newtype Row = Row Object
   deriving (Eq, Show)

instance FromJSON Row where
    parseJSON (Object v) = 
         pure Row <* (v .: "_id" :: Parser Text)
                  <*> pure v
    parseJSON _ = fail "row should be an object"

instance ToJSON Row where
   toJSON (Row o) = Object o

rowId :: Row -> Text
rowId (Row o) = case parse (.: "_id") o of
                  Success v -> v
                  Error msg -> error $ "rowId failed " ++ msg

lookupRow :: FromJSON a => Text -> Row -> Result a
lookupRow field (Row o) = parse (.: field) o 

newRow :: Id -> Object -> IO Row
newRow id_ obj = do
        return $ Row 
               $ HashMap.insert "_id" (toJSON id_)
               $ obj 
                     
-----------------------------------

type Table = HashMap Text Row 

data TableUpdate
        = RowUpdate Row       -- update a row with a new row, by _id.
        | RowDelete Id        -- consider previous updates to row deleted
        deriving (Show, Eq)
        
instance ToJSON TableUpdate where
   toJSON (RowUpdate row)      = toJSON row
   toJSON (RowDelete key)      = object ["delete"   .= key]     -- no _id key

instance FromJSON TableUpdate where
    parseJSON (Object v) = 
         RowUpdate <$> parseJSON (Object v) <|> 
         RowDelete <$> v .: "delete"
    parseJSON _ = error "TableUpdate Object was not a valid Object"

