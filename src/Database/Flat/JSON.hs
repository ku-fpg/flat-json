{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, LambdaCase, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes #-}
module Database.Flat.JSON where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.Aeson as Aeson

import           Data.Aeson hiding (Object)
import           Data.Aeson.Types hiding (Object)
import           Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Lazy as LBS

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           Data.Char (isDigit)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Time.Clock

import           System.IO

import           Database.Flat.JSON.Types
import           Database.Flat.JSON.Table

import           Control.Natural
import           Control.Transformation
import           Control.Object

data CRUDF :: * -> * where
  CreateRowF :: Aeson.Object       -> CRUDF Row
  ReadRowF   :: Id                 -> CRUDF (Maybe Row)
  UpdateRowF :: Id -> Aeson.Object -> CRUDF () 
  DeleteRowF :: Id                 -> CRUDF ()

instance CreateRow CRUDF where
  createRow = CreateRowF

instance ReadRow CRUDF where
  readRow   = ReadRowF
  readTable = undefined

instance UpdateRow CRUDF where
  updateRow = UpdateRowF 
  deleteRow = DeleteRowF

{-
  actorCRUD :: Table 
          -> (TableUpdate -> IO ()) -- single-threaded callback for updating
	  -> IO (Object CRUDF)
actorCRUD env push = do

    table :: TMVar Table <- newTMVarIO env
    
    let top :: STM Integer
        top = do t <- readTMVar table
                 return $ foldr max 0
                          [ read (Text.unpack k)
                          | k <- HashMap.keys t
                          , Text.all isDigit k
                          ]

    uniq <- atomically $ do
               mx <- top
               newTVar (mx + 1)

    -- Get the next, uniq id when creating a row in the table.
    let next :: STM Text           
        next = do
               n <- readTVar uniq
               let iD = Text.pack (show n) :: Text
               t <- readTMVar table
               if HashMap.member iD t
               then do mx <- top
                       writeTVar uniq (mx + 1)
                       next
                 -- Great, we can use this value
               else do writeTVar uniq $! (n + 1)
                       return iD

    let updateCRUD update = do
          tab <- atomically $ takeTMVar table 
          push update                     -- how do we handle failure here?
          atomically $ putTMVar table $ tableUpdate update $ tab
          -- we do not return until the update has been commited to
          -- both the internal and external state

    return $ Object $ Nat $ \ case 
       CreateRowF obj -> do id_ <- atomically $ next -- this feels wrong, allocation without locking
                            row <- newRow id_ obj
                            updateCRUD (RowUpdate row)
                            return row

       ReadRowF id_    -> atomically $ 
                               do t <- readTMVar table
                                  return $ HashMap.lookup id_ t

          -- if you insert names with your own ids, make sure they never clash with the generated ones.
       UpdateRowF id_ obj -> do row <- newRow id_ obj
                                updateCRUD $ RowUpdate row

       DeleteRowF id_     -> updateCRUD $ RowDelete id_

-- | Use a flat file to generate a persistent CRUD Object.
persistentCRUD :: Bool -> FilePath -> IO (Object CRUDF)
persistentCRUD online fileName = do
        h <- openBinaryFile fileName ReadWriteMode
        -- Read what you can, please, into a Table.
        tab <- hReadTable h 

        -- TODO: check for EOF & writeable, etc
        
        -- close then hadle
        if online
        then actorCRUD tab $ onlineTableUpdate h 
        else do hClose h
                actorCRUD tab $ offlineTableUpdate fileName


{-
creator :: (TableReader f, TableUpdater f, TableAllocator f) => Object f -> IO (Object RowCreator)


appender :: TableUpdater f => Object f -> Object RowUpdater
-}

-}

data RowReader :: * -> * where
  ReadRow :: Id -> RowReader (Maybe Row)

instance ReadRow RowReader where
  readRow   = ReadRow
  readTable = undefined

reader :: TableReader f => Object (f Table) -> (Object RowReader)
reader o = Object $ Nat $ \ case 
  ReadRow id_ -> do
          tab <- o # tableReader
          return $ HashMap.lookup id_ tab

instance TableType Table where
