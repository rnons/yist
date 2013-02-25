module Handler.User where

import Import

data Username = Username {
    usernameIdent :: Text
    }

userForm :: Form Username
userForm = renderDivs $ Username
    <$> areq textField "Username" Nothing
    
getUserR :: UserId -> Handler RepHtml
getUserR userId = do
    entries <- runDB $ selectList [EntryAuthorId ==. userId] [Desc EntryPosted]
    defaultLayout $ do
        $(widgetFile "user")

getNewUserR :: Handler RepHtml
getNewUserR = do
    Just (Entity _ user) <- maybeAuth
    
    case userIdent user of
         Just _ -> redirect HomeR
         Nothing -> do
             (entryWidget, enctype) <- generateFormPost userForm
             defaultLayout $ do
                 $(widgetFile "username")

postNewUserR :: Handler RepHtml
postNewUserR = do
    Just (Entity _ user) <- maybeAuth
    Just uid <- maybeAuthId
    ((result, entryWidget), enctype) <- runFormPost userForm
    case result of 
         FormSuccess username -> do
             runDB $ replace uid $ User (userEmail user) (Just $ usernameIdent username) Nothing
             redirect HomeR
         _ -> defaultLayout $ do
             $(widgetFile "username")
