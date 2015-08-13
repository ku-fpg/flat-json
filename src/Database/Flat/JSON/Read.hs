{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Database.Flat.JSON.Read  where

import           Control.Object
import           Control.Natural

import           Database.Flat.JSON.Types
import           Database.Flat.JSON.Table

import           System.IO

---------------------------------------------------------

data TableRead :: * -> * where TableRead :: TableRead Table

instance TableReader TableRead where
  tableReader = TableRead

-- Read the file once into the *heap*,  then return the same table 
heapTableReadOnly :: FilePath -> IO (Object TableRead)
heapTableReadOnly fileName = do
        h <- openBinaryFile fileName ReadMode
        tab <- hReadTable h
        hClose h
        return $ Object $ Nat $ \ TableRead -> return tab

-- Read the *file* each time a Table is requested
fileTableReadOnly :: FilePath -> Object TableRead
fileTableReadOnly fileName = Object $ Nat $ \ TableRead -> do
        h <- openBinaryFile fileName ReadMode
        tab <- hReadTable h
        hClose h
        return tab

