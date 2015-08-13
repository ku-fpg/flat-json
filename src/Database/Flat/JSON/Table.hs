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

type Table = HashMap Text Row 

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


tableUpdate :: TableUpdate -> Table -> Table
tableUpdate (RowUpdate row) = HashMap.insert (rowId row) row
tableUpdate (RowDelete key) = HashMap.delete key
tableUpdate (Shutdown _msg) = id

-----------------------------------------------------------

readTable :: Handle -> IO Table
readTable = foldTable tableUpdate HashMap.empty

foldTable :: (TableUpdate -> db -> db) -> db -> Handle -> IO db
foldTable f z h = do

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

readTableUpdates :: Handle -> IO [TableUpdate]
readTableUpdates h = foldTable (:) [] h >>= return . reverse

writeTableUpdate :: Handle -> TableUpdate -> IO ()
writeTableUpdate h row = do
        LBS.hPutStr h (encode row)
        LBS.hPutStr h "\n" -- just for prettyness, nothing else
                     
writeTable :: Handle -> Table -> IO ()
writeTable h table = sequence_
        [ writeTableUpdate h $ RowUpdate row    -- assuming the invarient that "_id" is the index
        | (_,row) <- HashMap.toList table
        ]

persistanceTableUpdate  :: Handle -> TableUpdate -> IO ()
persistanceTableUpdate h up = do
        writeTableUpdate h up
        case up of
          Shutdown {} -> hClose h
          _           -> hFlush h

offlineTableUpdate :: FilePath -> TableUpdate -> IO ()
offlineTableUpdate fileName up = do
        h <- openBinaryFile fileName ReadWriteMode
        writeTableUpdate h up
        hClose h
         

-- TODO: what happens if the TableUpdate contains _|_?
-- Perhaps there should be a deepseq requrement on the argument?
writeableTableUpdate :: Handle -> IO (TableUpdate -> IO ())
writeableTableUpdate h = do
    updateChan <- newTChanIO

    -- This is in its own thread, to make it thread safe
    let loop = do
          (tu,done) <- atomically $ do
                  readTChan updateChan
--          print $ "writing" ++ show tu
          LBS.hPutStr h (encode tu)
          LBS.hPutStr h "\n" -- just for prettyness, nothing else
          hFlush h
          case tu of
             Shutdown {} -> do
                     hClose h
                     atomically $ putTMVar done ()
                     return ()
             _ -> do atomically $ putTMVar done ()
                     loop

    _ <- forkIO $ loop
    
    return $ \ up -> do
        done <- newTMVarIO ()
        atomically $ writeTChan updateChan (up,done)
        atomically $ takeTMVar done



externalTableUpdate :: (TableUpdate -> IO ()) -> IO (TableUpdate -> IO ())
externalTableUpdate k = do
    updateChan <- newTChanIO

    let loop = do
          (tu,done) <- atomically $ do
                  readTChan updateChan
--          print $ "writing" ++ show tu
          k tu
          atomically $ putTMVar done ()
          case tu of
             Shutdown {} -> return ()
             _           -> loop

    _ <- forkIO $ loop
    
    return $ \ up -> do
        done <- newTMVarIO ()
        atomically $ writeTChan updateChan (up,done)
        atomically $ takeTMVar done