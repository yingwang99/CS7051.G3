module Handler.Submit where


import Data.Aeson
import Data.Aeson.TH
--import Data.Bson.Generic
import GHC.Generics
--import Servant


postsubmitR :: Handler Html
postsubmitR =
    do defaultLayout $
        do let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "search")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''


doCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< env h p)


doLoadEnvVars :: Maybe String -> Maybe String -> Maybe String -> IO ()
doLoadEnvVars s = doCall $ loadEnvVars s

doPerformRestCall :: Maybe String -> Maybe String -> Maybe String -> IO ()
doPerformRestCall s  =  doCall $ performRestCall s



opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName

  return $ info (   helper
                <*> subparser
                       (  command "load-envs"
                                  (withInfo ( doLoadEnvVars
                                            <$> optional (strOption ( long "name"
                                                                   <> short 'n'
                                                                   <> help "The variable to load."))
                                            <*> serverIpOption
                                            <*> serverPortOption) "Load an environment variable on the remote server." )
                       <> command "rest-call"
                                   (withInfo ( doPerformRestCall
                                           <$> optional (strOption ( long "search"
                                                                  <> short 's'
                                                                  <> help "The search string for the hackage call."))
                                           <*> serverIpOption
                                           <*> serverPortOption) "Do a hackage rest call from the remote server." )))




serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                      <> short 'i'
                                      <> metavar "IPADDRESS"
                                      <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")
