{-# LANGUAGE FlexibleContexts, FlexibleInstances,GADTs,OverloadedStrings,TypeFamilies #-} 
import Web.Scotty.Trans as S
import Web.Scotty.Hastache
import Text.Hastache 
import Network.Wai.Middleware.Static ( staticPolicy, noDots, addBase, (>->) )
import Database.Persist
import qualified Data.Text.Lazy as TL
import Text.Hastache.Context
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B 
import Network.Wai.Parse
import System.FilePath
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Directory
import Data.Time.Clock
import System.IO

import Dao
import Data
import Models
import Framework
import DataTransport

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        scottyH 3000 website 

website :: ScottyH ()
website = do 
            liftIO buildDb
            setTemplatesDir "templates"
            middleware $ staticPolicy (noDots >-> addBase "static")
            S.get "/" $ go rHome
            S.post "/" $ go rHomePost
            S.get "/login" $ go rLogin
            S.post "/login" $ go rLoginPost
            S.get "/logout" $ go' rLogout
            S.get "/myItems/" $ go rMyItems
            S.get "/it/:id" $ go rItemDownload
            S.get "/items/:name" $ go $ join $ rItems <$> paramOrEmpty "name"
            S.get "/pic/:name" $ go' rPic
            S.get "/upload" $ go' rUpload
            S.get "/a/echo" $ go rEcho
            S.get "/a/cache" $ goApi rCache
            S.post "/a/echo" $ go rEcho
            S.get "/a/categs" $ go rCategs
            S.get "/a/maincategs" $ go rMainCategs
            S.get "/a/workspaces" $ go rWorkspaces
            S.post "/a/logout" $ goApi rLogoutApi
            --S.get "/a/myimages" $ serve $ auth' rMyImagesApi
            S.post "/a/upload" $ goApi' rUploadPost
            S.post "/a/login" $ goApi rLoginApi
            S.get "/a/reg"  $ goApi rRegisterApi
            S.post "/a/postitem" $ goApi' $ join $ rSaveItem <$> 
                    paramOrDefault "title" "No title" <*>
                    paramOrEmpty "desc" <*>
                    maybeParam "cid" <*> 
                    maybeParam "fkey" <*> 
                    maybeParam "pub" 
            where 
              go = serve userBySessKey
              go' = serve' userBySessKey
              goApi :: GenericDTO a => ApiA a -> ActionH ()
              goApi = serveApi userBySessKey
              goApi' :: GenericDTO a => ApiA a -> ActionH ()
              goApi' = serveApi' userBySessKey

rCategs :: WebA ()
rCategs = getCategoriesByLevel 1 >>= ok . buildCombo

userBySessKey :: WebA (Maybe String) -> WebA (Maybe (Entity User))
userBySessKey s = s >>= maybe (return Nothing) (userBySess . TL.pack)

rCache :: ApiA EmptyDTO
rCache = do
  r1 <- cacheSetObj "some key" [ComboItem "ana" "are" "mere", ComboItem "mama" "are" "pere"]
  r2 <- cacheSetStr "some other key" "ana are 2 mere"
  liftIO $ do 
          either print print r1
          either print print r2
  return $ Right EmptyDTO

rPic :: WebA()
rPic = do
    u <- currentUser
    maybeParam "name" >>= maybe 
        (out $ uploadPath </> "notfound.jpg") 
        (\f -> out $ uploadPath </> showKey u </> str f <.> "jpg")
        where 
            out = lift . S.file

buildDownUrl :: FilePath -> FilePath
buildDownUrl = (++) "/pic/"  

rMainCategs :: WebA ()
rMainCategs = getCategoriesByLevel 0 >>= ok . buildCombo

rWorkspaces :: WebA ()
rWorkspaces = getWorkspaces >>= ok . buildCombo

rHome :: WebA ()
rHome = do
    fmtList getWorkspaces >>= bindT "cmbWorks" 
    fmtList (getCategoriesByLevel 0) >>= bindT "cmbMainCats" 
    fmtList (getCategoriesByLevel 1) >>= bindT "cmbCats" 
    render "home"
        
fmtList :: (ComboClass a) => WebA [Entity a] -> WebA (MuType IO)
fmtList getter = do
    ws <- map toComboItem <$> getter 
    return $ MuList (map mkGenericContext ws)

rHomePost :: WebA ()
rHomePost = do
    login >>= either (void . return) (bindT "usr" . MuVariable . email)
    fwd "/"

rMyImagesApi :: WebA()
rMyImagesApi = render "myimages"

rEcho :: WebA () 
rEcho = do
    printPars
    p <- lift S.params
    lift $ S.json $ foldl fc TL.empty p
    where 
        fc a n = TL.append a (snd n)

rRegisterApi :: ApiA LoginDTO
rRegisterApi = do
    v <- validateCreds <$> maybeParam "e" <*> maybeParam "p"
    case v of 
      Right (e,p) -> add (newusr e p) >> rLoginApi
      Left m -> return $ Left $ ErrDTO 1 m
    where 
        fn = TL.takeWhile (/= '@')
        newusr e p = User e p e 0 (fn e) TL.empty (fn e)

rLogin :: WebA ()
rLogin = render "login"

rLoginPost :: WebA ()
rLoginPost = do
    r <- login 
    case r of
        Left (ErrDTO _ m) -> do
            bindT "error" $ MuVariable m
            render "login"
        Right _ -> fwd "/"

rLoginApi :: ApiA LoginDTO
rLoginApi = login 
        
login :: ApiA LoginDTO
login = do
    v <- validateCreds <$> maybeParam "e" <*> maybeParam "p"  
    case v of 
        Right (e,p) -> userByEmail e >>= maybe 
            (return $ Left $ ErrDTO 1 "Email not found") 
            (matchp p) 
        Left m -> return $ Left $ ErrDTO 1 m
    where 
        matchp p usr = do
            let u = entityVal usr
            if userPassword u == p
                then do
                    s <- newSession usr
                    refreshAuthCookie (str $ sessionKey s) 3600
                    return $ Right $ LoginDTO (userEmail u) (sessionKey s) 
                else return $ Left $ ErrDTO 1 "Password doesn't match"

newSession :: Entity User -> WebA Session
newSession usr = do
    guid <- newGuid
    now <- liftIO getCurrentTime
    delSessionByUserId (mkInt $ entityKey usr)
    let s = Session guid now (entityKey usr ) 3600
    void $ add s 
    return s

validateCreds :: Maybe TL.Text -> Maybe TL.Text -> Either TL.Text (TL.Text, TL.Text)
validateCreds e p  
    | isNothing e = Left "Empty email"
    | isNothing p = Left "Empty password"
    | otherwise = Right (fromJust e, fromJust p)

printPars :: WebA ()
printPars = do
    allPars <- lift S.params 
    liftIO $ print $ foldl fmt TL.empty allPars 
    where
        fmt a n = TL.append a (snd n)

rLogout :: WebA()
rLogout = do
  u <- currentUser 
  delSessionByUserId $ mkInt $ entityKey u
  removeAuthCookie >> fwd "/"

rLogoutApi :: ApiA EmptyDTO
rLogoutApi = do 
    u <- currentUser
    delSessionByUserId $ mkInt $ entityKey u 
    removeAuthCookie 
    return $ Right EmptyDTO

rUpload :: WebA ()
rUpload = do
    registerCss "/css/fileuploader.css"
    registerScript "/js/fileuploader.js"
    registerScript "/js/Upload.js"
    fmtList (getCategoriesByLevel 0) >>= bindT "cmbMainCats" 
    fmtList (getCategoriesByLevel 1) >>= bindT "cmbCats" 
    render "upload"

rUploadPost :: ApiA UploadDTO
rUploadPost = do
    b <- lift S.body
    r <- saveFile b 
    case r of 
      Left er -> return $ Left $ ErrDTO 1 (pck er)
      Right url -> return $ Right $ UploadDTO (buildDownUrl $ fname url) True
    where
        fname = dropExtension . takeFileName

rUploadApi :: ApiA UploadDTO
rUploadApi = do 
    fs <- lift S.files 
    valid fs
    where 
        valid :: [S.File] -> ApiA UploadDTO
        valid [f] = do 
            k <- saveFile (fileContent $ snd f) 
            case k of
              Left er -> return $ Left $ ErrDTO 1 $ pck er
              Right url -> return $ Right $ UploadDTO url True
        valid _ =  return $ Left $ ErrDTO 1 "Wrong files"

saveFile :: B.ByteString -> WebA (Either String String)
saveFile d = do
    guid <- newGuid
    u <- currentUser
    liftIO $ store d $ AFile (str guid) $ showKey u

rSaveItem :: ParamA -> ParamA -> MaybeParamA -> MaybeParamA -> MaybeParamA -> ApiA EmptyDTO
rSaveItem title desc cat guid visible = do
    let vis = isJust visible
    u <- currentUser
    case vparams guid cat of
      Left m -> return $ Left $ ErrDTO 1 m
      Right (g,c) -> do 
            let f = AFile (str g) $ showKey u
            v <- valid f (read $ str c)
            case v of
              Right ac -> do
                void $ add $ Item title desc (entityKey ac) 
                              (entityKey u) (pck $ fpath f) vis
                return $ Right EmptyDTO 
              Left m -> return $ Left $ ErrDTO 1 m
    where
        vparams :: Maybe TL.Text -> Maybe TL.Text -> Either TL.Text (TL.Text, TL.Text)
        vparams g c = maybe 
                        (Left "Category missing") 
                        (const $ maybe 
                            (Left "Key missing") 
                            (const $ Right (fromJust g, fromJust c)) g) c
        valid :: AFile -> Int -> WebA (Either TL.Text (Entity Category))
        valid af cid = do
            fex <- liftIO $ fexists af
            if not fex 
                then return $ Left "File doesn't exists"
                else do
                    c <- getCategoryById cid
                    return $ maybe (Left "Category not found") Right c

rItems :: ParamA -> WebA ()
rItems n = do
    registerCss "/css/items.css"
    registerScript "/js/masonry.pkgd.min.js"
    userByNick n >>= renderUserPage

renderUserPage ::Maybe (Entity User) -> WebA ()
renderUserPage Nothing = errPage' "Sorry dude, no user found"
renderUserPage (Just usr) = do 
        its <- getItemsByWorkspaceAndUser 609 (mkInt $ entityKey usr)
        let dtos = map (dto usr) its
        bindT "items" $ MuList (map mkGenericContext dtos)
        render "items"
    where 
      dto :: Entity User -> Entity Item -> ItemDTO
      dto u it = 
        let i = entityVal it 
            uid = mkInt $ entityKey u
            unick = userNick $ entityVal u in 
        ItemDTO (mkInt $ entityKey it) (itemUrl it) (itemTitle i) (itemDesc i) uid unick ""

itemUrl :: Entity Item -> TL.Text
itemUrl it = "/it/" +++ pck ( showKey it) 

rMyItems :: WebA()
rMyItems = do
    registerCss "/css/items.css"
    registerScript "/js/masonry.pkgd.min.js"
    currentUser >>= renderUserPage . Just 

rItemDownload :: WebA ()
rItemDownload = 
    maybeParam "id" >>= maybe 
        (out $ uploadPath </> "notfound.jpg") 
        url
        where 
            url :: TL.Text -> WebA()
            url iid = do 
              i <- entityVal <$> getItemById (read $ str iid)
              let fn = takeBaseName (str $ itemPath i) ++ "_thumb" <.> "jpg"
                  fp = uploadPath </> keyToStr ( itemUser i) </> fn
              fex <- liftIO $ doesFileExist fp 
              out (if fex then fp else uploadPath </> "notfound.jpg") 
            out = lift . S.file

buildCombo :: (ComboClass a) => [Entity a] -> ComboDTO
buildCombo vals = ComboDTO (map toComboItem vals)
