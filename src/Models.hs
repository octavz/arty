{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, TypeFamilies, TypeSynonymInstances, NoMonomorphismRestriction, DeriveGeneric, DeriveDataTypeable, EmptyDataDecls, QuasiQuotes, TemplateHaskell, RankNTypes #-}

module Models where

--import Database.Persist.MongoDB as Mongo
--import qualified Database.MongoDB as DB ( PortID(PortNumber) )
import Database.Persist.TH
import Data.Time.Clock
import Data.Text.Lazy
import qualified Data.Text.Lazy as TL ( Text )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"][persistLowerCase|
User
    email Text
    password Text
    openId Text
    openIdType Int
    fname Text
    lname Text
    nick Text
    UniqueNick nick
    UniqueEmail email
    deriving Show
Session
    key Text
    added UTCTime
    user UserId
    exp Int
    deriving Show
Workspace
    title String
    desc String
    deriving Show
Category
    title String
    desc String
    workspace WorkspaceId
    parent CategoryId Maybe
    level Int
    deriving Show
Item
    title TL.Text
    desc TL.Text
    cat CategoryId
    user UserId
    path TL.Text
    public Bool
    deriving Show
Folder
    title String
    desc String
    cover String
    user UserId
FolderItem
    folder FolderId 
    item ItemId 
|]
