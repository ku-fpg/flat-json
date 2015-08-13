{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs, LambdaCase #-}
module Database.Flat.JSON.Update where

import           Control.Object
import           Control.Natural
import           Control.Transformation

import           Database.Flat.JSON.Types
import           Database.Flat.JSON.Table
import qualified Database.Flat.JSON.Read as R

import           System.IO

---------------------------------------------------------

data TableReadUpdate :: * -> * where 
        TableRead   ::                TableReadUpdate Table
        TableUpdate :: TableUpdate -> TableReadUpdate ()

instance TableReader TableReadUpdate where
  tableReader = TableRead

instance TableUpdater TableReadUpdate where
  tableUpdater = TableUpdate

fileTableUpdate :: FilePath -> Object TableReadUpdate
fileTableUpdate fileName = Object $ Nat $ \ case
    TableRead -> R.fileTableReadOnly fileName # R.TableRead
    TableUpdate up -> do
            h <- openBinaryFile fileName AppendMode
            hWriteTableUpdate h up
            hClose h

