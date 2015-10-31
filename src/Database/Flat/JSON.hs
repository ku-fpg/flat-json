{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, LambdaCase, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes #-}
module Database.Flat.JSON where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Parser as P
import qualified Data.Attoparsec.ByteString as Atto
import           Data.Aeson.Types 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.List
import           Data.String
import           Data.Text(Text)
import qualified Data.Text as Text



import           System.IO


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

----------------------------------------------------------

tableUpdate :: TableUpdate -> Table -> Table
tableUpdate (RowUpdate row) = HashMap.insert (rowId row) row
tableUpdate (RowDelete key) = HashMap.delete key

-----------------------------------------------------------

-- do not export

hRollTable :: Table -> Handle -> IO Table
hRollTable = hFoldTable tableUpdate

hFoldTable :: (TableUpdate -> db -> db) -> db -> Handle -> IO db
hFoldTable f z h = do

    let sz = 32 * 1024 :: Int

    let loadCRUD bs env
          | BS.null bs = do
                  bs' <- BS.hGet h sz
                  if BS.null bs'
                  then return env        -- done, done, done (EOF)
                  else loadCRUD bs' env
          | otherwise =
                  parseCRUD (Atto.parse P.json bs) env
        parseCRUD (Atto.Fail bs _ msg) env
                | BS.all (isSpace . chr . fromIntegral) bs = loadCRUD BS.empty env
                | otherwise = fail $ "parse error: " ++ msg
        parseCRUD (Atto.Partial k) env = do
                  bs <- BS.hGet h sz    
                  parseCRUD (k bs) env
        parseCRUD (Atto.Done bs r) env = do
                  case fromJSON r of
                    Error msg -> error msg
                    Success update -> loadCRUD bs $! f update env

    loadCRUD BS.empty z

hReadTableUpdates :: Handle -> IO [TableUpdate]
hReadTableUpdates h = hFoldTable (:) [] h >>= return . reverse

hWriteTableUpdate :: Handle -> TableUpdate -> IO ()
hWriteTableUpdate h row = do
        LBS.hPutStr h (encode row)
        LBS.hPutStr h "\n" -- just for prettyness, nothing else

hReadTable :: Handle -> IO Table
hReadTable = hFoldTable tableUpdate HashMap.empty
                     
hWriteTable :: Handle -> Table -> IO ()
hWriteTable h table = sequence_
        [ hWriteTableUpdate h $ RowUpdate row    -- assuming the invarient that "_id" is the index
        | (_,row) <- HashMap.toList table
        ]


sqlSortBy :: Text -> [Row] -> [Row]
sqlSortBy nm rows = map snd $ sortBy cmp $ prep
          where
                 cmp  a b = fst a `compare` fst b

                 key :: Row -> Maybe SortKey
                 key row = SortKey <$> rowLookup nm row

                 prep :: [(Maybe SortKey,Row)]
                 prep = map (\ v -> (key v,v)) rows



sqlWhere :: Text -> (SortKey -> Bool) -> Table -> Table
sqlWhere nm op tab = HashMap.filter f tab
  where
      f :: Row -> Bool
      f row = case rowLookup nm row of
               Just value -> op (SortKey value)
               Nothing -> False

------------------------------------------------------------------------------------

newtype SortKey = SortKey Value
  deriving (Eq,Show)

instance Ord SortKey where
  compare (SortKey (String s1)) (SortKey (String s2)) = s1 `compare` s2
  compare (SortKey (Number s1)) (SortKey (Number s2)) = s1 `compare` s2
  compare s1 s2 = show s1 `compare` show s2

instance Num SortKey where
  (+) = error "(+)"
  (-) = error "(-)"
  (*) = error "(*)"
  abs = error "abs"
  signum = error "signum"

  fromInteger = SortKey . Number . fromInteger

instance Fractional SortKey where
  recip = error "recip"
  fromRational = SortKey . Number . fromRational

instance IsString SortKey where
  fromString = SortKey . String . fromString


like :: SortKey -> Text -> Bool
like (SortKey (String s)) ms = case parsePercent ms of
    (False,txt,True ) -> txt `Text.isPrefixOf` s
    (True, txt,False) -> txt `Text.isSuffixOf` s
    (True, txt,True ) -> txt `Text.isInfixOf` s
    (False,txt,False) -> txt == s
like _ _ = False

parsePercent :: Text -> (Bool,Text,Bool)
parsePercent txt0 = (hd,txt2,tl)
  where
    (hd,txt1) = if not (Text.null txt0) && Text.head txt0 == '%'
                then (True,Text.tail txt0)
                else (False,txt0)

    (txt2,tl) = if not (Text.null txt1) && Text.last txt1 == '%'
                then (Text.init txt1,True)
                else (txt1,False)
