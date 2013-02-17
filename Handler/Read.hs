{-# LANGUAGE ConstraintKinds #-}
module Handler.Read where

import Control.Monad.Reader
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

-- Get all revisions of an entry.
getRevisionR :: EntryId -> Handler RepHtml
getRevisionR entryId = do
    entry <- runDB $ get404 entryId
    muser <- maybeAuth
    repo <- liftIO $ openLgRepository (entryRepoPath entryId)
    contents <- liftIO $ withOpenLgRepository repo $ do
        let master = "refs/heads/master"
        Just masterRef <- resolveRef master
        
        mc <- resolveCommit masterRef
        parents <- getAllParents mc
        liftIO $ print $ length parents
        let revisions = mc:parents
        forM revisions $ \p -> do
            Just centry <- commitEntry p (fromText $ entryTitle entry)
            pentry <- identifyEntry p centry
            let t = renderOid $ Tagged (pinnedOid pentry)
            c <- catBlobUtf8 t
            return (c, show $ pinnedOid pentry)

    -- Use `print` here to be strict.
    liftIO $ print contents
    defaultLayout $ do
        $(widgetFile "revision")

        
