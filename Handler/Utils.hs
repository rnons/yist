module Handler.Utils where

import Data.Time (getCurrentTime)
import Import hiding (FilePath)
import Filesystem.Path.CurrentOS 

import Git
import Git.Libgit2

getCurrentUserId :: Handler UserId
getCurrentUserId = do
    muser <- maybeAuthId
    case muser of
         Just uid -> return uid
         Nothing -> undefined

-- | Is it possible to get UserId from User, I mean without DB query
getCurrentUser :: Handler User
getCurrentUser = do
    muser <- maybeAuth
    case muser of
         Just (Entity _ user) -> return user
         Nothing -> undefined

-- | The Signature is need in every commit. The User is passed in.
-- Or should I retrieve User in function body?
getCurrentUserSig :: User -> LgRepository Signature
getCurrentUserSig user = do
    now  <- liftIO getCurrentTime
    return Signature {
               signatureName  = userEmail user
             , signatureEmail = userEmail user
             , signatureWhen  = now }

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
    <*> aformM getCurrentUser
    <*> aformM getCurrentUserId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" Nothing

updateForm :: Entry -> Form Entry
updateForm entry = renderDivs $ Entry
    <$> areq textField "Title" (Just $ entryTitle entry)
    <*> aformM getCurrentUser
    <*> aformM getCurrentUserId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" (Just $ entryContent entry)

entryRepoPath :: EntryId -> FilePath
entryRepoPath entryId = repoDir </> (fromText $ toPathPiece entryId) <.> "git"

entryFilePath :: EntryId -> Text -> String
entryFilePath entryId title = encodeString $ entryRepoPath entryId </> 
                                   fromText title
