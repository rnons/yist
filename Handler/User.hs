module Handler.User where

import Import

data Username = Username {
    usernameIdent :: Text
    }

userForm :: Form Username
userForm = renderDivs $ Username
    <$> areq textField "Username" Nothing
    
{-
getUserR :: UserId -> Handler RepHtml
getUserR userId = do
    entries <- runDB $ selectList [EntryAuthorId ==. Just userId] [Desc EntryPosted]
    defaultLayout $ do
        $(widgetFile "user")
-}

getUserR :: Text -> Handler RepHtml
getUserR name = do
    entries <- runDB $ do
        mu <- getBy $ UniqueUserName name
        case mu of
             Nothing -> lift notFound 
             Just _ -> do
                 selectList [EntryAuthorName ==. name] [Desc EntryPosted]
    defaultLayout $ do
        $(widgetFile "user")

getNewUserR :: Handler RepHtml
getNewUserR = do
    Just (Entity _ user) <- maybeAuth
    case userName user of
         "" -> do
             (entryWidget, enctype) <- generateFormPost userForm
             defaultLayout $ do
                 $(widgetFile "username")
         _  -> redirect HomeR

postNewUserR :: Handler RepHtml
postNewUserR = do
    Just (Entity _ user) <- maybeAuth
    Just uid <- maybeAuthId
    ((result, entryWidget), enctype) <- runFormPost userForm
    case result of 
         FormSuccess username -> do
             runDB $ replace uid $ User (userEmail user) (usernameIdent username) 
             redirect HomeR
         _ -> defaultLayout $ do
             $(widgetFile "username")
