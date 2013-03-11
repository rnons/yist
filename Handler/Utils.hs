module Handler.Utils where

import Data.Maybe (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Filesystem.Path.CurrentOS 
import qualified Filesystem.Path.CurrentOS as P

import Git
import Git.Libgit2

import Import

getCurrentUserIdent :: Handler (Maybe Text)
getCurrentUserIdent = do
    muser <- maybeAuth
    case muser of
         Just (Entity _ user) -> return $ userIdent user
         Nothing -> undefined

-- | The Signature is need in every commit. The User is passed in.
-- Or should I retrieve User in function body?
getCurrentUserSig :: User -> LgRepository IO Signature
getCurrentUserSig user = do
    now  <- liftIO getCurrentTime
    return Signature {
               signatureName  = fromJust $ userIdent user
             , signatureEmail = userEmail user
             , signatureWhen  = now }

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" 
                       { fsId = Just "title"
                       , fsAttrs = [("class", "span8")]
                       } Nothing
    <*> aformM getCurrentUserIdent
    <*> aformM maybeAuthId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" 
                           { fsId = Just "content"
                           , fsAttrs = [("class", "span8"), ("rows", "12")]
                           } Nothing

updateForm :: Entry -> Form Entry
updateForm entry = renderDivs $ Entry
    <$> areq textField "Title" (Just $ entryTitle entry)
    <*> aformM getCurrentUserIdent
    <*> aformM maybeAuthId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" 
                           { fsId = Just "content"
                           , fsAttrs = [("class", "span8"), ("rows", "12")]
                           } (Just $ entryContent entry)

entryRepoPath :: EntryId -> P.FilePath
entryRepoPath entryId = repoDir </> (fromText $ toPathPiece entryId) <.> "git"

entryFilePath :: EntryId -> Text -> String
entryFilePath entryId title = encodeString $ entryRepoPath entryId </> 
                                   fromText title
