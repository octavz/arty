{-# LANGUAGE FlexibleContexts, FlexibleInstances,GADTs,OverloadedStrings,TypeFamilies,DeriveDataTypeable,RankNTypes #-} 
module Framework where

import Control.Applicative 
import Web.Scotty.Trans as S
import Web.Scotty.Hastache
import Text.Hastache 
import qualified Data.ByteString.Char8 as BS 
import qualified Data.Text.Encoding as TEnc 
import qualified Data.Text.Lazy.Encoding as TLEnc 
import Web.Cookie ( parseCookies )
import Database.Persist
import qualified Data.Text.Lazy as TL
import Text.Hastache.Context
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Data.Maybe
import Data.Data
import Data.UUID ( toString )
import Data.UUID.V4 ( nextRandom )
import Control.Monad
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL

import Models
import DataTransport

newGuid :: WebA TL.Text
newGuid = (TL.pack . toString) <$> liftIO nextRandom

data HData = HData { cUser :: Maybe (Entity User), css :: [String], scripts :: [String]} 
data TempRes = TempRes { src :: String } deriving (Data, Show, Typeable)

type WebA a = ST.StateT HData ActionH a
type ApiA a = WebA (Either ErrDTO a)
type ParamA = TL.Text
type MaybeParamA = (Maybe TL.Text)

templateExt :: String
templateExt = ".mustache"

authCookieName :: String
authCookieName = "art_sid"

sessCookieName :: String
sessCookieName = "art_sess"

(+++) :: TL.Text -> TL.Text -> TL.Text
(+++) = TL.append

pck :: String -> TL.Text 
pck = TL.pack

str :: TL.Text -> String
str = TL.unpack

fwd :: TL.Text -> WebA a
fwd = lift . redirect

serveInternal :: (WebA a -> ActionH a)
serveInternal = flip ST.evalStateT (HData Nothing [] [] ) 

serve :: (WebA (Maybe String) -> WebA (Maybe (Entity User))) -> WebA a -> ActionH a
serve authCallback handler = serveInternal $ setStateUser authCallback >> handler

serveApi :: GenericDTO a => (WebA (Maybe String) -> WebA (Maybe (Entity User))) -> ApiA a -> ActionH ()
serveApi authCallback handler = serve authCallback $ ok' handler

serveApi' :: GenericDTO a => (WebA (Maybe String) -> WebA (Maybe (Entity User))) -> ApiA a -> ActionH ()
serveApi' authCallback handler = serveApi authCallback apiVal
  where 
    apiVal = setStateUser authCallback >> getStateUser >>= maybe (return $ Left $ ErrDTO 3 "No Session.") (const handler)
   
serve' :: (WebA (Maybe String) -> WebA (Maybe (Entity User))) -> WebA a -> ActionH a
serve' authCallback handler = serveInternal $ do
  setStateUser authCallback
  getStateUser >>= maybe (fwd "/login") (const handler)

setStateUser :: (WebA (Maybe String) -> WebA (Maybe (Entity User)))-> WebA ()
setStateUser authCallback = do
  u <- authCallback sessKey
  case u of 
    Nothing -> return ()
    Just _ -> do 
      fromJust <$> sessKey >>= flip refreshAuthCookie 3600  -- refresh cookie
      s <- ST.get 
      ST.put s{ cUser = u} -- save global user
      return ()

registerCss :: String -> WebA ()
registerCss v = do
    s <- ST.get
    ST.put s{ css = v : css s }

registerScript :: String -> WebA ()
registerScript v = do
    s <- ST.get
    ST.put s{ scripts = v : scripts s }

errPage' :: String -> WebA ()
errPage' msg = do
    bindT "message" $ MuVariable msg
    render "error"

ok :: (GenericDTO a) => a -> WebA ()
ok = lift . S.json 

ok' :: GenericDTO a => ApiA a -> WebA ()
ok' v = v >>= either (lift .S.json) (lift . S.json)

bindT :: String -> MuType IO -> WebA ()
bindT a = lift . setH a

getStateUser :: WebA (Maybe (Entity User))
getStateUser = cUser <$> ST.get

currentUser :: WebA (Entity User)
currentUser = fromJust <$> getStateUser

renderStr :: TL.Text -> WebA ()
renderStr = lift . S.html

renderStr' :: B.ByteString -> WebA ()
renderStr' = lift . S.html . TLEnc.decodeUtf8 . BL.fromStrict

render :: String -> WebA ()
render view = do
    getStateUser >>= maybe (return ()) addLoginData 
    s <- ST.get
    bindT "css" $ MuList (map (mkGenericContext . TempRes) $ css s)
    bindT "script" $ MuList (map (mkGenericContext . TempRes) $ scripts s)
    newGuid >>= addSessionCookie . TL.unpack
    lift $ hastache $ view ++ templateExt
    where
        addLoginData u = bindT "usr" $ MuVariable (userEmail $ entityVal u) 

paramOrEmpty :: TL.Text -> WebA TL.Text
paramOrEmpty n = lift $ rescue (S.param n) (const $ return TL.empty)

paramOrDefault :: TL.Text -> TL.Text -> WebA TL.Text
paramOrDefault n d = lift $ rescue (S.param n) (const $ return d)

maybeParam :: TL.Text -> WebA (Maybe TL.Text)
maybeParam n = lift $ rescue (Just <$> S.param n) (const $ return Nothing)

refreshAuthCookie :: String -> Integer -> WebA ()
refreshAuthCookie s ex = 
    lift $ S.setHeader "Set-Cookie" $ 
        pck $ authCookieName ++ "=" ++ s ++ "; Max-Age=" ++ show ex
    --where fmtExp = pck $ 
    --formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" (sessionExp s)

removeAuthCookie :: WebA ()
removeAuthCookie = lift $ S.setHeader "Set-Cookie" $ 
    pck $ authCookieName ++ "; expires=Thu, 01 Jan 1970 00:00:00 GMT"

addSessionCookie :: String -> WebA ()
addSessionCookie v = 
    getCookie sessCookieName 
        >>= maybe 
            (lift $ addHeader "Set-Cookie" $ pck $ sessCookieName ++ "=" ++ v) 
            return . void

sessKey :: WebA (Maybe String)
sessKey = getCookie authCookieName

sessCookie :: WebA (Maybe String)
sessCookie = getCookie sessCookieName

getCookie :: String -> WebA (Maybe String)
getCookie k = lift $ extract <$> S.reqHeader "Cookie" 
    where
        isSess a = fst a == BS.pack k
        dec = fmap (TEnc.encodeUtf8 . TL.toStrict)
        extract :: Maybe TL.Text -> Maybe String
        extract h = do
                allc <- parseCookies <$> dec h
                let f = filter isSess allc
                if null f then Nothing 
                    else Just $ BS.unpack $ snd $ head f
