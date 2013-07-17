module Handler.Create where

import           Control.Exception (finally)
import           Control.Monad.Reader
import           Data.Maybe (fromJust)
import           Git
import qualified Git.Libgit2 as Lg
import           Filesystem.Path.CurrentOS 

import Import
import Handler.Utils

getCreateR :: Handler Html
getCreateR = do
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "create")

postCreateR :: Handler Html
postCreateR = do
    muser <- maybeAuth
    let user = entityVal $ fromJust muser
    ((result, entryWidget), enctype) <- runFormPost entryForm 
    case result of
         FormSuccess entry -> do
             entryId <- runDB $ insert entry
             --repo <- liftIO $ initRepo (entryRepoPath entryId) 
             --liftIO $ runLgRepository repo action
             liftIO $ do
                 startupBackend Lg.lgFactory
                 finally
                     (withNewRepository Lg.lgFactory (entryRepoPath entryId) $ do
                         action)
                     (Git.shutdownBackend Lg.lgFactory)
             redirect $ EntryR (entryAuthorName entry) entryId
           where 
             action = do
                 blob <- createBlobUtf8 (unTextarea $ entryIntro entry)
                 tr <- createTree $ putBlob (fromText $ entryTitle entry) blob
                 sig <- getCurrentUserSig user
                 --c <- createCommit [] (treeRef tr) sig sig "Created" Nothing
                 c <- createCommit [] tr sig sig "Created" Nothing
                 updateReference_ "refs/heads/master" (RefObj (commitRef c))
                 updateReference_ "HEAD" (RefSymbolic "refs/heads/master")

                 {-
                 blob <- createBlobUtf8 (unTextarea $ entryContent entry)
                 tr <- newTree
                 putBlob tr (fromText $ entryTitle entry) blob
                 sig <- getCurrentUserSig user
                 c <- createCommit [] (treeRef tr) sig sig "Created" Nothing
                 updateRef_ "refs/heads/master" (RefObj (commitRef c))
                 updateRef_ "HEAD" (RefSymbolic "refs/heads/master")
                 -}
         _ -> defaultLayout $ do
            $(widgetFile "create")
