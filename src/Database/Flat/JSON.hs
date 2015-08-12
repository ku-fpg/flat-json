{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, LambdaCase, TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes #-}
module Database.Flat.JSON where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Aeson
import           Data.Aeson.Types
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

data CRUDr :: * -> * where
  CreateRow :: Object       -> CRUDr Row
  ReadRow   :: Id           -> CRUDr (Maybe Row)
  UpdateRow :: Id -> Object -> CRUDr () 
  DeleteRow :: Id           -> CRUDr ()

newtype CRUD = CRUD (forall a. CRUDr a -> IO a)

instance Transformation CRUDr IO CRUD where
  CRUD o # g = Nat o # g

instance Crud CRUDr where
  createRow = CreateRow
  readRow   = ReadRow   
  updateRow = UpdateRow 
  deleteRow = DeleteRow 
 

actorCRUD :: (TableUpdate -> IO ())	   
	 -> Table               -- initial Table
	 -> IO CRUD
actorCRUD push env = do

    table <- newTVarIO env
    
    let top :: STM Integer
        top = do t <- readTVar table
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
               t <- readTVar table
               if HashMap.member iD t
               then do mx <- top
                       writeTVar uniq (mx + 1)
                       next
                 -- Great, we can use this value
               else do writeTVar uniq $! (n + 1)
                       return iD

    let updateCRUD update = do
          atomically $ modifyTVar table (tableUpdate update)
          push update

    return $ CRUD $ \ case 
       CreateRow  obj -> do id_ <- atomically $ next
                            row <-  newRow id_ obj
                            updateCRUD (RowUpdate row)
                            return row

       ReadRow id_    -> atomically $ 
                               do t <- readTVar table
                                  return $ HashMap.lookup id_ t

       UpdateRow id_ obj -> do row <- newRow id_ obj
                               updateCRUD $ RowUpdate row

       DeleteRow id_     -> updateCRUD $ RowDelete id_
