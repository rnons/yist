module Handler.User where

import Import

getUserR :: UserId -> Handler RepHtml
getUserR userId = do
    entries <- runDB $ selectList [EntryAuthorId ==. userId] [Desc EntryPosted]
    defaultLayout $ do
        $(widgetFile "user")
