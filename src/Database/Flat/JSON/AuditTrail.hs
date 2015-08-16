{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs, LambdaCase #-}
module Database.Flat.JSON.AuditTrail 
        ( AuditTrail
        , openAuditTrail
        ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Natural
import           Control.Object as O


import qualified Data.Aeson as Aeson
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

import           Database.Flat.JSON.Types
import           Database.Flat.JSON.Table

import           System.IO
import           Text.Read


data AuditTrail :: * -> * where
  CreateRow :: Aeson.Object -> AuditTrail Row
  Close    :: Text          -> AuditTrail ()

instance CreateRow AuditTrail where
  createRow = CreateRow

instance CloseResource AuditTrail where
  closeResource = Close

-- Open an audit trail. No way of closing/releasing this.
-- flushed after *every* createRow.

openAuditTrail :: FilePath -> IO (Object AuditTrail)
openAuditTrail fileName = do
        h <- openBinaryFile fileName ReadWriteMode
        let f (RowUpdate (Row row)) n =
                 case HashMap.lookup "_id" row of
                    Just (Aeson.String txt) -> 
                      case readMaybe (Text.unpack txt) of
                        Just (n' :: Integer) -> max n n'
                        Nothing              -> n
                    _ -> n
            f _               n = n
        n :: Integer <- hFoldTable f (1 :: Integer) h
        hClose h
        var <- newTMVarIO n
        return $ O.Object $ Nat $ \ case
           CreateRow o -> do
                  bracket 
                     (atomically $ takeTMVar var)
                     (\ uq -> atomically $ putTMVar var $! succ uq)
                     (\ uq -> do
                          row <- newRow (Text.pack $ show uq) o
                          hWriteTableUpdate h $ RowUpdate row
                          hFlush h
                          return row)
           Close msg -> do
                  bracket 
                     (atomically $ takeTMVar var)
                     (\ uq -> atomically $ putTMVar var $! uq)
                     (\ _ -> do
                          hWriteTableUpdate h $ Shutdown msg
                          hFlush h
                          hClose h
                          return ())
