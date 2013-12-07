module Main where

import Data.List
import System.Environment
import System.Exit
import System.IO
import System.Process

import qualified Blaze.ByteString.Builder as BB
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp


main :: IO ()
main = do
    port <- fmap (read . head) getArgs
    run port $ \req -> case (requestMethod req, pathInfo req) of
        ("GET", ref:path@(_:_)) -> commandResponse "git"
            ["show", T.concat [ref, ":", T.concat $ intersperse "/" path]]
        (_, _) -> return $ responseLBS status404 [] "404: No such route"

commandResponse :: Text -> [Text] -> IO Response
commandResponse exec args = responseSourceBracket command close source
  where
    command = do
        (_, Just outh, Just errh, ph) <- createProcess
            (proc (T.unpack exec) (map T.unpack args))
                { std_out = CreatePipe, std_err = CreatePipe }
        return (outh, errh, ph)
    close (outh, errh, ph) = do
        _ <- waitForProcess ph
        hClose outh
        hClose errh
    source (outh, errh, ph) = do
        (st, h) <- flip fmap (waitForProcess ph) $ \ex -> case ex of
            ExitSuccess -> (status200, outh)
            ExitFailure _ -> (status400, errh)
        return
            ( st, []
            , CB.sourceHandle h $= CL.map (Chunk . BB.fromByteString) )
