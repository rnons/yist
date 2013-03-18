module Handler.Update where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Git
import Git.Utils
import Git.Libgit2
import Filesystem.Path.CurrentOS 

import Import
import Handler.Utils


getUpdateR :: Text -> EntryId -> Handler RepHtml
getUpdateR authorName entryId = do
    entry <- runDB $ get404 entryId
    (entryWidget, enctype) <- generateFormPost $ updateForm entry
    defaultLayout $ do
        $(widgetFile "update")
        
postUpdateR :: Text -> EntryId -> Handler RepHtml
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
                 Just cParent <- resolveRef masterRef
                 blob <- createBlobUtf8 (unTextarea $ entryContent entry)
                 tr <- newTree
                 putBlob tr (fromText $ entryTitle entry) blob
                 sig <- getCurrentUserSig user
                 c <- createCommit [cParent] (treeRef tr) sig sig
                                  "Updated" (Just masterRef)
                 updateRef_ "refs/heads/master" (RefObj (commitRef c))
         _ -> defaultLayout $ do
            $(widgetFile "create")
