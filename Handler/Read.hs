module Handler.Read where

import Control.Monad.Reader
import Data.Algorithm.Diff
import qualified Data.Text as T
import Data.List (head)
import Filesystem.Path.CurrentOS 
import Git
import Git.Libgit2

import Import
import Handler.Utils

getEntryR :: T.Text -> EntryId -> Handler Html
getEntryR authorName entryId = do
    entry <- runDB $ get404 entryId
    muserId <- maybeAuthId
    defaultLayout $ do
        $(widgetFile "read")

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
                                    

   

        
