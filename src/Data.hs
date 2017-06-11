{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data where

import Database.Persist
--import Database.Persist.MongoDB as Mongo
--import qualified Database.MongoDB as DB ( PortID(PortNumber) )
import Database.Persist.Postgresql
import Data.Text.Lazy
import Control.Monad.Trans
import System.Directory
import Database.Redis
import Data.Aeson

import Graphics.Thumbnail
import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString as BS 
import System.FilePath
import Models
import Framework

data AFile = AFile { uKey :: String, uId :: String} deriving (Show)

fpath :: AFile -> FilePath
fpath f = uploadPath </> uId f </> (uKey f ++ ".jpg")

uploadPath :: String
uploadPath = "/Users/octav/Projects/arty/files"

class GenericFile a where
    createFile :: String -> String -> a
    store :: B.ByteString -> a -> IO (Either String String)
    fexists :: a -> IO Bool

instance GenericFile AFile where
  createFile = AFile
  fexists = doesFileExist. fpath
  
  store bs f = do 
    let p = fpath f 
        thfn = takeBaseName p ++ "_thumb" <.> "jpg"
        thp = takeDirectory p </> thfn
    createDirectoryIfMissing True (takeDirectory p)  
    B.writeFile p bs 
    res <- mkThumbnail' ((0,0),(200,200)) bs 
    case res of
      Left err -> return $ Left err
      Right th -> do 
        B.writeFile thp (lbs th) 
        return $ Right p

connStr :: ConnectionString
connStr = "host=localhost dbname=arty user=postgres password=root port=5432"

--runDb query = Mongo.withMongoDBConn dbname hostname port' Nothing time $ Mongo.runMongoDBPoolDef query
--        where
--          dbname = T.pack "arty" :: Mongo.Database
--          hostname = "localhost" :: Mongo.HostName
--          port' = DB.PortNumber 27017 :: Mongo.PortID
--          time = 3600 :: NominalDiffTime
--runDb query = withPostgresqlPool connStr 1 $ \pool -> do flip runSqlPersistMPool pool $  query


runDb :: SqlPersistM a -> WebA a
runDb query = liftIO $ withPostgresqlConn connStr $ runSqlPersistM query

runDbIO :: SqlPersistM a -> IO a
runDbIO query = withPostgresqlConn connStr $ runSqlPersistM query

buildDb :: IO ()
buildDb = runDbIO $ do
            --deleteWhere ( [] :: [Filter Session] )
            --deleteWhere ( [] :: [Filter User])
            deleteWhere ( [] :: [Filter Item])
            deleteWhere ( [] :: [Filter Category])
            --deleteWhere ( [] :: [Filter Workspace])
            deleteWhere ( [] :: [Filter Folder])

            runMigration migrateAll

            --wid <- insert $ Workspace "Images" "" 
            --_ <- insert $ Workspace "Templates" "" 
            --_ <- insert $ Workspace "Iconsets" "" 
            
            let wid = keyStr "609"
            lid <- insert $ Category "Landscapes" "" wid Nothing 0
            stid <- insert $ Category "Street" "" wid Nothing 0
            aid <- insert $ Category "Animals" "" wid Nothing  0
            mid <- insert $ Category "Macro" "" wid Nothing 0

            bid <- insert $ Category "Birds" "" wid (Just aid) 1
            insert $ Category "Flowers" "" wid (Just mid) 1
            insert $ Category "Bugs" "" wid (Just mid) 1
            insert $ Category "People" "" wid (Just stid) 1 
            insert $ Category "Portraits" "" wid (Just stid) 1
            insert $ Category "Waterscapes" "" wid (Just lid) 1
            insert $ Category "City Lines" "" wid (Just lid) 1

            --insert $ User "a" "a" "a" 0 "octav" "zaharia" "octav"
            let fullPath = uploadPath </> "5"
            files <- liftIO $ getDirectoryContents fullPath
            let allJpegs = Prelude.filter (\f -> 
                            ".jpg" == takeExtension f && 
                            (not $ isInfixOf "thumb" $ pck f)) 
                            files
            --liftIO $ print allJpegs
            mapM_ (\f -> insert $ 
              Item "Some title" "Small description" bid (keyStr "5") 
              (pack $ fullPath </> f) True) allJpegs
            return ()

keyStr :: PersistEntity a => String -> Key a
keyStr i = Key $ PersistInt64 (fromIntegral (read i :: Integer))
-- read -- oidToKey . read

defaultCacheTimeout :: Integer
defaultCacheTimeout = 100

cacheSetObj :: ToJSON a => BS.ByteString -> a -> WebA (Either Reply Status)
cacheSetObj key val = liftIO $ do
  con <- connect defaultConnectInfo
  runRedis con $ setex key defaultCacheTimeout (B.toStrict $ encode val)

cacheSetStr :: BS.ByteString -> BS.ByteString -> WebA (Either Reply Status)
cacheSetStr key val = liftIO $ do
  con <- connect defaultConnectInfo
  runRedis con $ setex key defaultCacheTimeout val
  
cacheGet :: BS.ByteString -> WebA (Either Reply (Maybe BS.ByteString))
cacheGet key = liftIO $ do
  con <- connect defaultConnectInfo
  runRedis con $ Database.Redis.get key  
