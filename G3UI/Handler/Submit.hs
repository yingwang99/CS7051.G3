module Handler.Submit where


import Data.Aeson
import Data.Aeson.TH
--import Data.Bson.Generic
import GHC.Generics
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Data.FileEmbed (embedFile)

--import Servant

-- postSubmitR :: Handler Html
-- postSubmitR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postSubmitR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing
--     defaultLayout $ do
--       let (username) = resultIds
--       aDomId <- newIdent
--       setTitle "Welcome To Yesod!"
--       $(widgetFile "results")
postSubmitR :: Handler Value
postSubmitR =
    error "The simple scaffolding does not support authentication or a database for storing comments"

commentIds2 :: (Text, Text, Text, Text, Text,Text, Text)
commentIds2 = ("js-username", "js-reponame", "js-urlname","js-tokenname", "js-repoType", "js-queryType", "js-selectname")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

resultIds :: (Text)
resultIds = ("js-username")

reportExceptionOr act b =  b >>= \ b' ->
  case b' of
    -- Left err -> putStrLn $ ''"Call failed with error: " ++ show err'
     Right b'' ->  act b''


--doCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< env h p)


--doLoadEnvVars :: Maybe String -> Maybe String -> Maybe String -> IO ()
--doLoadEnvVars s = doCall $ loadEnvVars s

--doPerformRestCall :: Maybe String -> Maybe String -> Maybe String -> IO ()
--doPerformRestCall s  =  doCall $ performRestCall s



--opts :: IO (ParserInfo (IO ()))
--opts = do
  --progName <- getProgName

  --return $ info (   helper
    --            <*> subparser
      --                 (  command "load-envs"
        --                          (withInfo ( doLoadEnvVars
          --                                  <$> optional (strOption ( long "name"
            --                                                       <> short 'n'
              --                                                     <> help "The variable to load."))
                --                            <*> serverIpOption
                  --                          <*> serverPortOption) "Load an environment variable on the remote server." )
                    --   <> command "rest-call"
                      --             (withInfo ( doPerformRestCall
                        --                   <$> optional (strOption ( long "search"
                          --                                        <> short 's'
                            --                                      <> help "The search string for the hackage call."))
                              --             <*> serverIpOption
                                --           <*> serverPortOption) "Do a hackage rest call from the remote server." )))




--serverIpOption :: Parser (Maybe String)
--serverIpOption = optional $ strOption ( long "ip"
  --                                    <> short 'i'
    --                                  <> metavar "IPADDRESS"
      --                                <> help "the ip address of the use-haskell service.")

--serverPortOption :: Parser (Maybe String)
--serverPortOption = optional $ strOption (  long "port"
  --                                      <> short 'n'
    --                                    <> metavar "PORT_NUMBER"
      --                                  <> help "The port number of the use-haskell service.")
