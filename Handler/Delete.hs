module Handler.Delete where

import System.Directory          (removeDirectoryRecursive)
import Filesystem.Path.CurrentOS (encodeString)

import Import
import Handler.Utils

postDeleteR :: EntryId -> Handler RepHtml
postDeleteR  entryId = do
    runDB $ delete entryId
    liftIO $ removeDirectoryRecursive $ encodeString $ entryRepoPath entryId
    redirect HomeR
