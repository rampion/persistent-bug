{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT)
import Data.Text (unpack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person
  name String
  age Int Maybe
  deriving Show
|]

selectAndShow :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend Person backend) => [Filter Person] -> ReaderT backend m ()
selectAndShow filters = do
  liftIO . putStrLn $ "people that match filters: " <> show filters
  liftIO . print . fmap (entityVal) =<< selectList filters []

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll

  _johnId <- insert $ Person "John Doe" $ Just 35
  _janeId <- insert $ Person "Jane Doe" Nothing

  selectAndShow []
  selectAndShow [PersonAge <-. [Nothing, Just 35]]
  selectAndShow [PersonAge <-. [Just 35]]
  selectAndShow [PersonAge <-. [Nothing]]

  deleteWhere @_ @_ @Person []

instance Show (Filter Person) where
  showsPrec = \p -> \case
    Filter PersonId filterValue filterFilter -> showParen (p > 4) do
      showString "PersonId" . showFilter filterFilter . showValue filterValue
    Filter PersonName filterValue filterFilter -> showParen (p > 4) do
      showString "PersonName" . showFilter filterFilter . showValue filterValue
    Filter PersonAge filterValue filterFilter -> showParen (p > 4) do
      showString "PersonAge" . showFilter filterFilter . showValue filterValue
    FilterAnd filters -> showString "FilterAnd " . shows filters
    FilterOr filters -> showString "FilterOr " . shows filters
    BackendFilter _ -> showString "BackendFilter _"
    where
      showFilter :: PersistFilter -> ShowS
      showFilter =
        showString . \case
          Eq -> " ==. "
          Ne -> " /=. "
          Gt -> " >. "
          Lt -> " <. "
          Ge -> " >=. "
          Le -> " <=. "
          In -> " <-. "
          NotIn -> " </-. "
          BackendSpecificFilter t -> " `" <> unpack t <> "` "

      showValue :: Show typ => FilterValue typ -> ShowS
      showValue = \case
        FilterValue val -> showsPrec 5 val
        FilterValues vals -> showsPrec 5 vals
        UnsafeValue val -> showsPrec 5 (toPersistValue val)
