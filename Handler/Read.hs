module Handler.Read where

import Import

getEntryR :: EntryId -> Handler RepHtml
getEntryR entryId = do
    entry <- runDB $ get404 entryId
    muser <- maybeAuth
    defaultLayout $ do
        $(widgetFile "read")

