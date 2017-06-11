{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Dao where

import Database.Esqueleto 
import Control.Applicative
import qualified Data.Text.Lazy as TL

import Models
import Framework
import Data

getItemsByWorkspaceAndUser :: Int -> Int -> WebA [Entity Item]
getItemsByWorkspaceAndUser wid uid = runDb $ 
  select $ 
  from $ \(item, cat, work ) -> do
  where_ $
    item ^. ItemUser ==. valkey (fromIntegral uid) &&. 
    work ^. WorkspaceId ==. valkey (fromIntegral wid) &&.
    item ^. ItemCat ==. cat ^. CategoryId &&. 
    cat ^. CategoryWorkspace ==. work ^. WorkspaceId
  return item

getItemById :: Int -> WebA (Entity Item)
getItemById iid = 
  head <$> (runDb $ 
    select $ from $ \item -> do
    where_ $ item ^. ItemId ==. valkey (fromIntegral iid)
    return item)

getCategories :: WebA [Entity Category]
getCategories = runDb $ select $ from $ \cat -> return cat
 
getWorkspaces :: WebA [Entity Workspace] 
getWorkspaces = runDb $ select $ from $ \work -> return work

getCategoriesByLevel :: Int -> WebA [Entity Category]
getCategoriesByLevel lvl =  runDb $ --selectList [CategoryLevel ==. lvl] []
  select $ from $ \cat -> do
  where_ $ cat ^. CategoryLevel ==. val (fromIntegral lvl)
  return cat

getCategoriesByWorkspace :: Int -> WebA [Entity Category]
getCategoriesByWorkspace wid = runDb $ -- selectList [CategoryWorkspace ==. keyStr key] []
  select $ from $ \cat -> do
  where_ $ cat ^. CategoryWorkspace ==. valkey (fromIntegral wid)
  return cat
        
getItemsByUser :: Int -> WebA [Entity Item]
getItemsByUser uid = runDb $ --selectList [ItemUser ==. entityKey u] []
  select $ from $ \item -> do
  where_ $ item ^. ItemUser ==. valkey (fromIntegral uid)
  return item

getCategoryById :: Int -> WebA (Maybe (Entity Category))
getCategoryById cid = maybeHead <$> (runDb $ --selectFirst [CategoryId ==. keyStr cid] [] 
  select $ from $ \cat -> do
  where_ $ cat ^. CategoryId ==. valkey (fromIntegral cid)
  return cat)
  
maybeHead :: [a] -> Maybe a
maybeHead xs
  | null xs = Nothing
  | otherwise = Just $ head xs

userBySess :: TL.Text -> WebA (Maybe (Entity User))
userBySess sk = maybeHead <$> (runDb $
  select $ from $ \(sess, user) -> do
    where_ $
      sess ^. SessionUser ==. user ^. UserId &&.
      sess ^. SessionKey ==. val sk
    return user)

userByEmail :: TL.Text -> WebA (Maybe (Entity User))
userByEmail = runDb . getBy . UniqueEmail

userByNick :: TL.Text -> WebA (Maybe (Entity User))
userByNick = runDb . getBy . UniqueNick

add :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => a -> WebA (Key a)
add = runDb . insert

delSessionByUserId :: Int -> WebA ()
delSessionByUserId uid = runDb $ --deleteWhere [SessionUser ==. entityKey u]
  delete $ from $ \sess -> where_ $ sess ^. SessionUser ==. valkey ( fromIntegral uid)


