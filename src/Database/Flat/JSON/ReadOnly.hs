{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs, LambdaCase #-}
module Database.Flat.JSON.ReadOnly  where

import           Control.Object
import           Control.Natural

import           Database.Flat.JSON.Types
import           Database.Flat.JSON.Table

import           System.IO

---------------------------------------------------------

data TableRead :: * -> * -> * where TableRead :: TableType s => TableRead s s

instance TableReader TableRead where
  tableReader = TableRead

-- DO NOT LIKE THIS
-- Read the handle ever time the object method is called.
hTableReadOnly :: forall t . TableType t => Handle -> Object (TableRead t)
hTableReadOnly h = Object $ Nat $ \ TableRead -> do
        tab :: t <- hRollTable h
        return tab

-- Read the file once into the *heap*,  then return the same value each time.
heapTableReadOnly :: forall t . TableType t => FilePath -> IO (Object (TableRead t))
heapTableReadOnly fileName = do
        h <- openBinaryFile fileName ReadMode
        tab :: t <- hRollTable h
        hClose h
        return $ Object $ Nat $ \ case
          TableRead -> return tab

-- Read the *file* each time a Table is requested
fileTableReadOnly :: forall t . TableType t => FilePath -> Object (TableRead t)
fileTableReadOnly fileName = Object $ Nat $ \ TableRead -> do
        h <- openBinaryFile fileName ReadMode
        tab :: t <- hRollTable h
        hClose h
        return tab
