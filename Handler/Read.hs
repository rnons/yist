module Handler.Read where

import qualified Control.Exception as E
import Control.Monad.Reader
import Data.Algorithm.Diff
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.HashMap.Strict as Map
import Data.List (head)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS 
import Git
import Git.Libgit2
import Network.HTTP.Types.Status
import Network.HTTP.Conduit
import Text.HTML.DOM
import Text.XML.Cursor

import Import
import Handler.Utils

getEntryR :: T.Text -> EntryId -> Handler Html
getEntryR authorName entryId = do
    entry <- runDB $ get404 entryId
    muserId <- maybeAuthId
    defaultLayout $ do
        $(widgetFile "read")

data UrlInfo = UrlInfo 
    { url :: Text
    , title :: Text
    , error :: Text
    }

instance ToJSON UrlInfo where
    toJSON (UrlInfo u t e) =
        object [ "url" .= u
               , "title" .= t
               , "error" .= e
               ]

checkUrl url = E.catch
    (do req <- parseUrl $ T.unpack url
        withManager $ \manager -> do
            res <- http req manager
            let st = responseStatus res
                hdrMap = Map.fromList (responseHeaders res)

            title <- 
                if "text/html; charset=" `B.isPrefixOf` (hdrMap Map.! "Content-Type")
                    then do
                        doc <- responseBody res $$+- sinkDoc
                        return $ head $ fromDocument doc $// element "title" &// content                   
                    else do
                        responseBody res $$+- return ()
                        return url

            if st == status200 
                then return $ Just $ UrlInfo url title ""
                else return Nothing)
    (\e -> do print (e :: E.SomeException)
              return Nothing)

postCheckUrlR :: Handler Value
postCheckUrlR = do
    url <- lookupPostParam "url"
    case url of
         Just url' -> do
             vurl <- lift $ checkUrl url'
             case vurl of
                 Just vurl' -> returnJson vurl'
                 _          -> returnJson $ UrlInfo url' "" "Invalid"
         _         -> returnJson $ UrlInfo "" "" "Invalid"

getAllParents :: Git.Commit (LgRepository IO) 
              -> (LgRepository IO) [Git.Commit (LgRepository IO)]
getAllParents c = do
    p <- getCommitParents c
    if Import.null p 
       then return p
       else fmap (p++) $ getAllParents (head p)

-- | `getDiff` to every revision
-- Is it possible to rewrite this to use `scanr`?
revDiff :: [[Text]] -> [[Diff Text]]
revDiff [] = []
revDiff [x] = [getDiff [] x]
revDiff (x:xs) = getDiff (head xs) x : revDiff xs

data Version = Version
    { yistBid :: Text
    , yistName :: String
    , yistContent :: Text
    } deriving (Show)

-- Get all revisions of an entry.
getRevisionR :: Text -> EntryId -> Handler Html
getRevisionR authorName entryId = do
    entry <- runDB $ get404 entryId
    muser <- maybeAuth
    mid <- maybeAuthId
    repo <- liftIO $ openLgRepository (entryRepoOptions entryId)
    yist <- liftIO $ runLgRepository repo $ do
        let master = "refs/heads/master"
        Just masterRef <- resolveReference master
        mc <- resolveCommitRef masterRef
        parents <- getAllParents mc
        liftIO $ print $ length parents
        let commits = mc:parents
        forM commits $ \c -> do
            -- assume only one file in tree
            tr <- resolveTreeRef $ commitTree c
            (path, trEntry) <- fmap head (treeBlobEntries tr)
            pentry <- identifyEntry c trEntry
            let blobId = renderOid $ pinnedOid pentry
            content <- catBlobUtf8 blobId
            return Version { yistBid = blobId
                           , yistName = encodeString path
                           , yistContent = content
                           }
    let blobDiffs = revDiff $ map (T.lines . yistContent) yist
        prefixDiffs = map (T.unlines . prefixit) blobDiffs
        revisions = zip3 (map yistBid yist) (map yistName yist) prefixDiffs 
    -- Use `print` here to be strict.
    liftIO $ print yist
    defaultLayout $ do
        $(widgetFile "revision")
  where
    prefixit = map innermap
    innermap d = case d of
                      First l -> T.append "- " l
                      Second l -> T.append "+ " l
                      Both l _ -> T.append "  " l
                                    

   

        
