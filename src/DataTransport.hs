{-# LANGUAGE DeriveDataTypeable,TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataTransport where

import Database.Persist
import qualified Data.Text.Lazy as TL ( Text )
import Data.Aeson
import Data.Data
import Data.Aeson.TH ( defaultOptions, deriveJSON )

import Models

class (ToJSON a) => GenericDTO a where

data EmptyDTO = EmptyDTO deriving(Eq, Show)
data LoginDTO = LoginDTO { email :: TL.Text , session :: TL.Text} deriving(Eq, Show)
data ComboItem = ComboItem { cval :: String, cname :: String, cdata :: String} deriving (Show,Data,Typeable)
data ComboDTO = ComboDTO {items :: [ComboItem]} deriving (Show,Data,Typeable)
data ErrDTO = ErrDTO { errCode :: Int, errMsg :: TL.Text } deriving(Eq, Show)
data UploadDTO = UploadDTO { key :: String, success :: Bool } deriving (Data, Show, Typeable) 
data ItemDTO = ItemDTO {itId :: Int,itUrl::TL.Text, itTitle:: TL.Text, itDesc:: TL.Text, itUserId :: Int, itUserName::TL.Text, itExtra::TL.Text}
 deriving (Data, Show, Typeable) 

instance GenericDTO EmptyDTO 
instance GenericDTO LoginDTO 
instance GenericDTO ComboDTO 
instance GenericDTO UploadDTO 
instance GenericDTO ItemDTO 

$(deriveJSON defaultOptions ''EmptyDTO)
$(deriveJSON defaultOptions ''LoginDTO)
$(deriveJSON defaultOptions ''ComboItem)
$(deriveJSON defaultOptions ''ComboDTO)
$(deriveJSON defaultOptions ''ErrDTO)
$(deriveJSON defaultOptions ''UploadDTO)
$(deriveJSON defaultOptions ''ItemDTO)

class ComboClass a where
    ciKey :: Entity a -> String
    ciKey = showKey

    ciVal :: Entity a -> String
    ciData :: Entity a -> String

    toComboItem :: Entity a -> ComboItem
    toComboItem ent = ComboItem (ciKey ent) (ciVal ent) (ciData ent) 

instance ComboClass Category where
    ciVal = categoryTitle . entityVal 
    ciData ent =
            let ev = entityVal ent in
            case categoryParent ev of
                    Nothing -> show $ mkInt $ categoryWorkspace ev
                    Just akey -> show $ mkInt akey

instance ComboClass Workspace where
    ciVal = workspaceTitle . entityVal 
    ciData _ = ""


fromPersistValue' :: PersistField c => PersistValue -> c
fromPersistValue' = either (const $ error "Cast failed") id . fromPersistValue

mkInt :: Key a -> Int
mkInt = fromPersistValue' . unKey

showKey :: Entity e -> String
showKey = show . mkInt . entityKey

keyToStr :: Key e -> String
keyToStr = show . mkInt 
