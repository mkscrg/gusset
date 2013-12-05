module Main where

import Control.Exception
import Data.List
import System.Exit
import System.IO
import System.Process

import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Conduit (ResourceT)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp


main :: IO ()
main = run 5000 app

app :: Application
app req = case (requestMethod req, pathInfo req) of
    ("GET", ref:path@(_:_)) -> commandResponse "git"
        ["show", T.concat [ref, ":", T.concat $ intersperse "/" path]]
    (_, _) -> return $ ResponseBuilder status404 [] $
        BB.fromText "404: No such route"

commandResponse :: Text -> [Text] -> ResourceT IO Response
commandResponse exec args = do
    (sts, bs) <- liftIO $ fmap (either (status400, ) (status200, )) $
        command exec args
    return $ ResponseBuilder sts [] $ BB.fromByteString bs

command :: Text -> [Text] -> IO (Either ByteString ByteString)
command exec args = handleIO $ bracket cmd close check
  where
    handleIO = handle $ \e -> return $ Left $ B.pack $
        "IOException: " ++ show (e :: IOException)
    cmd = do
        (_, Just outh, Just errh, ph) <- createProcess
            (proc (T.unpack exec) (map T.unpack args))
                { std_out = CreatePipe, std_err = CreatePipe }
        return (outh, errh, ph)
    close (outh, errh, ph) = do
        _ <- waitForProcess ph
        hClose outh
        hClose errh
    check (outh, errh, ph) = do
        ex <- waitForProcess ph
        case ex of
            ExitSuccess -> fmap Right $ B.hGetContents outh
            ExitFailure _ -> fmap Left $ B.hGetContents errh
