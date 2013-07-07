{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuth
    entries <- runDB $ selectList [] [Desc EntryPosted]

    defaultLayout $ do
        $(widgetFile "homepage")

