-- import Prelude     (IO)
-- import Application (appMain)
--
-- main :: IO ()
-- main = appMain


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Conduit
import Network.Wai.Handler.Warp (runEnv)
import System.Environment (getEnv)
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Github

import qualified Data.Text as T

data OAuthKeys = OAuthKeys
    { oauthKeysClientId :: Text
    , oauthKeysClientSecret :: Text
    }

loadOAuthKeysEnv :: String -> IO OAuthKeys
loadOAuthKeysEnv prefix = OAuthKeys
    <$> (getEnvT $ prefix <> "_CLIENT_ID")
    <*> (getEnvT $ prefix <> "_CLIENT_SECRET")

  where
    getEnvT = fmap T.pack . getEnv

data App = App
    { appHttpManager :: Manager
    , appGithubKeys :: OAuthKeys
    }

mkYesod "App" [parseRoutes|
    / RootR GET
    /auth AuthR Auth getAuth
|]

instance Yesod App where
    -- redirect_uri must be absolute to avoid callback mismatch error
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    loginDest _ = RootR
    logoutDest _ = RootR

    -- Disable any attempt to read persisted authenticated state
    maybeAuthId = return Nothing

    -- Copy the Creds response into the session for viewing after
    authenticate c = do
        mapM_ (uncurry setSession) $
            [ ("credsIdent", credsIdent c)
            , ("credsPlugin", credsPlugin c)
            ] ++ credsExtra c

        return $ Authenticated "1"

    authHttpManager = appHttpManager

    authPlugins m =
        [ oauth2Github
            (oauthKeysClientId $ appGithubKeys m)
            (oauthKeysClientSecret $ appGithubKeys m)
        ]

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getRootR :: Handler Html
getRootR = do
    sess <- getSession

    defaultLayout [whamlet|
        <h2>
            <a href=@{AuthR LoginR}>Log in
            #{show sess}
    |]


mkFoundation :: IO App
mkFoundation = do

    appHttpManager <- newManager tlsManagerSettings
    appGithubKeys <- loadOAuthKeysEnv "GITHUB"

    return App{..}

main :: IO ()
main = runEnv 3000 =<< toWaiApp =<< mkFoundation
