module Handler.Create where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Git
import Git.Utils
import Git.Libgit2
import Filesystem.Path.CurrentOS 

import Import
import Handler.Utils

getCreateR :: Handler RepHtml
getCreateR = do
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "create")

postCreateR :: Handler RepHtml
postCreateR = do
    muser <- maybeAuth
    let user = entityVal $ fromJust muser
    ((result, entryWidget), enctype) <- runFormPost entryForm 
    case result of
         FormSuccess entry -> do
             entryId <- runDB $ insert entry
             repo <- liftIO $ createLgRepository (entryRepoPath entryId) True
             -- Bare repo is sufficient for now.
             --let filepath = entryFilePath entryId $ entryTitle entry
             --liftIO $ writeFile filepath 
             --                   (unTextarea $ entryContent entry)
             liftIO $ runReaderT (runLgRepository action) repo
             redirect $ EntryR entryId
           where 
             action = do
                 blob <- createBlobUtf8 (unTextarea $ entryContent entry)
                 tr <- newTree
                 putBlob tr (fromText $ entryTitle entry) blob
                 sig <- getCurrentUserSig user
                 c <- createCommit [] (treeRef tr) sig sig "Created" Nothing
                 updateRef_ "refs/heads/master" (RefObj (commitRef c))
                 updateRef_ "HEAD" (RefSymbolic "refs/heads/master")
         _ -> defaultLayout $ do
            $(widgetFile "create")
