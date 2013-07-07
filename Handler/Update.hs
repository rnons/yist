module Handler.Update where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Git
import Git.Libgit2
import Filesystem.Path.CurrentOS 

import Import
import Handler.Utils


getUpdateR :: Text -> EntryId -> Handler Html
getUpdateR authorName entryId = do
    entry <- runDB $ get404 entryId
    (entryWidget, enctype) <- generateFormPost $ updateForm entry
    defaultLayout $ do
        $(widgetFile "update")
        
postUpdateR :: Text -> EntryId -> Handler Html
postUpdateR authorName entryId = do
    muser <- maybeAuth
    let user = entityVal $ fromJust muser
    ((result, entryWidget), enctype) <- runFormPost entryForm
    case result of
         FormSuccess entry -> do
             runDB $ replace entryId entry
             repo <- liftIO $ openLgRepository (entryRepoOptions entryId)
             liftIO $ runLgRepository repo action
             redirect $ EntryR (entryAuthorName entry) entryId
           where 
             action = do
                 let masterRef = "refs/heads/master"
                 Just cParent <- resolveReference masterRef
                 blob <- createBlobUtf8 (unTextarea $ entryContent entry)
                 tr <- createTree $ putBlob (fromText $ entryTitle entry) blob
                 sig <- getCurrentUserSig user
                 c <- createCommit [cParent] tr sig sig
                                  "Updated" (Just masterRef)
                 updateReference_ "refs/heads/master" (RefObj (commitRef c))
         _ -> defaultLayout $ do
            $(widgetFile "create")
