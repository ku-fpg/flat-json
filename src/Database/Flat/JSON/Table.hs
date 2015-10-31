{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Database.Flat.JSON.Table where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Aeson
import           Data.Aeson.Parser as P
import           Data.Attoparsec.ByteString as Atto
import           Data.Char
--import           Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Database.Flat.JSON.Types

import           Data.Text(Text)
--import           Data.Time.Clock

import           System.IO

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
        parseCRUD (Fail bs _ msg) env
                | BS.all (isSpace . chr . fromIntegral) bs = loadCRUD BS.empty env
                | otherwise = fail $ "parse error: " ++ msg
        parseCRUD (Partial k) env = do
                  bs <- BS.hGet h sz    
                  parseCRUD (k bs) env
        parseCRUD (Done bs r) env = do
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

