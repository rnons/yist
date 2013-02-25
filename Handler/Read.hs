{-# LANGUAGE ConstraintKinds #-}
module Handler.Read where

import Control.Monad.Reader
import Data.Algorithm.Diff
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.List (head)
import Data.Tagged
import Filesystem.Path.CurrentOS 
import Git
import Git.Utils
import Git.Libgit2

import Import
import Handler.Utils

getEntryR :: EntryId -> Handler RepHtml
getEntryR entryId = do
    entry <- runDB $ get404 entryId
    muser <- maybeAuth
    defaultLayout $ do
        $(widgetFile "read")

--getAllParents :: Git.Repository m => Git.Commit m -> m [Git.Commit m]
getAllParents :: Git.Repository LgRepository => Git.Commit LgRepository -> LgRepository [Git.Commit LgRepository]
getAllParents c = do
    p <- getCommitParents c
    if Import.null p 
       then return p
       else fmap (p++) $ getAllParents (head p)

-- Is it possible to rewrite this to use `scanr`?
diff :: [[Text]] -> [[Diff Text]]
diff [] = []
diff [x] = [getDiff [] x]
diff (x:xs) = getDiff (head xs) x : diff xs

-- Get all revisions of an entry.
getRevisionR :: EntryId -> Handler RepHtml
getRevisionR entryId = do
    entry <- runDB $ get404 entryId
    muser <- maybeAuth
    repo <- liftIO $ openLgRepository (entryRepoPath entryId)
    blobContents <- liftIO $ withOpenLgRepository repo $ do
        let master = "refs/heads/master"
        Just masterRef <- resolveRef master
        mc <- resolveCommit masterRef
        parents <- getAllParents mc
        liftIO $ print $ length parents
        let commits = mc:parents
        forM commits $ \c -> do
            -- FixIt: doesn't work if `entryTitle` changed
            Just centry <- commitEntry c (fromText $ entryTitle entry)
            pentry <- identifyEntry c centry
            let blobId = renderOid $ Tagged (pinnedOid pentry)
            content <- catBlobUtf8 blobId
            return (content, blobId)
    let blobDiffs = diff $ map (T.lines . fst) blobContents
        prefixDiffs = map (T.unlines . prefixit) blobDiffs
        revisions = zip prefixDiffs $ map snd blobContents
    -- Use `print` here to be strict.
    liftIO $ print blobContents
    defaultLayout $ do
        $(widgetFile "revision")
  where
    prefixit = map innermap
    innermap d = case d of
                      First l -> T.append "-" l
                      Second l -> T.append "+" l
                      Both l _ -> T.append " " l
                                    

   

        
