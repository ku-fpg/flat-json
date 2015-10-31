{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ConstraintKinds, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Database.Flat.JSON.Types where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)

import           Data.Text(Text)

------------------------------------------------------------------------------------
-- Basic synonyms for key structures
--
-- | Every Row must have an '_id' field, of type 'Text'.
type Id        = Text

------------------------------------------------------------------------------------
-- | A Row is a Object, with "_id" (the unique identifier, a Text).

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

rowLookup :: Text -> Row -> Maybe Value
rowLookup field (Row o) = case parse (.: field) o of
                           Success v -> Just v
                           Error {}  -> Nothing

keysOfRow :: Row -> [Text]
keysOfRow (Row o) = HashMap.keys o 

newRow :: Id -> Object -> Row
newRow id_ obj = Row 
               $ HashMap.insert "_id" (toJSON id_)
               $ obj 
                     
rowToList :: Row -> [(Text,Value)]
rowToList (Row o) = HashMap.toList o

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

