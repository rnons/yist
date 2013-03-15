module Handler.Utils where

import Data.Time (getCurrentTime)
import Filesystem.Path.CurrentOS 
import qualified Filesystem.Path.CurrentOS as P

import Git
import Git.Libgit2

import Import

getCurrentUserName :: Handler Text
getCurrentUserName = do
    muser <- maybeAuth
    case muser of
         Just (Entity _ user) -> return $ userName user
         Nothing -> return "username not found"

-- | The Signature is need in every commit. The User is passed in.
-- Or should I retrieve User in function body?
getCurrentUserSig :: User -> LgRepository IO Signature
getCurrentUserSig user = do
    now  <- liftIO getCurrentTime
    return Signature {
               signatureName  = userName user
             , signatureEmail = userEmail user
             , signatureWhen  = now }

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" 
                       { fsId = Just "title"
                       , fsAttrs = [("class", "span8")]
                       } Nothing
    <*> aformM getCurrentUserName
    <*> aformM maybeAuthId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" 
                           { fsId = Just "content"
                           , fsAttrs = [("class", "span8"), ("rows", "12")]
                           } Nothing

updateForm :: Entry -> Form Entry
updateForm entry = renderDivs $ Entry
    <$> areq textField "Title" (Just $ entryTitle entry)
    <*> aformM getCurrentUserName
    <*> aformM maybeAuthId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" 
                           { fsId = Just "content"
                           , fsAttrs = [("class", "span8"), ("rows", "12")]
                           } (Just $ entryContent entry)

entryRepoPath :: EntryId -> P.FilePath
entryRepoPath entryId = repoDir </> (fromText $ toPathPiece entryId) <.> "git"

entryRepoOptions :: EntryId -> RepositoryOptions r
entryRepoOptions entryId = RepositoryOptions path True True undefined
  where
    path = repoDir </> (fromText $ toPathPiece entryId) <.> "git"

entryFilePath :: EntryId -> Text -> String
entryFilePath entryId title = encodeString $ entryRepoPath entryId </> 
                                   fromText title
