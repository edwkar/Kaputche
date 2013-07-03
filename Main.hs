import Prelude hiding                   (catch)

import Control.Applicative              ((<$>))
import Control.Concurrent               (forkIO)
import Control.Exception                (catch, IOException, evaluate)
import Control.Monad                    (forever, void, when, unless)
import Control.Monad.Error              (ErrorT, runErrorT, throwError)
import Control.Monad.IO.Class           (liftIO, MonadIO)
import Control.Monad.Reader             (ReaderT, runReaderT, ask)
import Data.ByteString                  (ByteString)
import Data.List                        (isInfixOf, sort)
import Network.Mime                     (MimeType, defaultMimeLookup)
import System.Environment               (getArgs)
import System.Exit                      (exitWith, ExitCode(..))
import System.IO                        (Handle, hPutStr, hPutStrLn, hClose,
                                         withFile, hGetContents,
                                         IOMode(ReadMode), stderr)

import qualified Data.ByteString.Char8  as B
import qualified Data.Text              as T
import qualified Network                as N
import qualified System.Directory       as D




type App = ReaderT ServerConfig IO

main :: IO ()
main = do
    args <- getArgs

    when (args /= []) $ do
        D.setCurrentDirectory (head args)
    cwd <- D.getCurrentDirectory
    putStrLn $ "Serving from directory '" ++ cwd ++ "'..."

    N.withSocketsDo $ runReaderT runServer conf
  where
    conf = ServerConfig (N.PortNumber 22880)

runServer :: App ()
runServer = do
    conf <- ask
    s <- lDoOrDie (N.listenOn $ serverPortId conf)
                 "Couldn't bind listening socket"
    forever $ acceptClient s
  where
    acceptClient s = do
        (h, _, _) <- lDoOrDie (N.accept s) "Failed to accept client"
        handleClient h

handleClient :: Handle -> App ()
handleClient h = liftIO $ void $ forkIO $ do
    reqRaw <- RawRequest <$> lines <$> hGetContents h
    res <- runErrorT $ processRequest reqRaw
    case res of
        Right r -> sendResponse h r
        Left e  -> sendResponse h $ HttpResponse httpBadRequest
                                                 htmlMimeType
                                                 (B.pack e)
    hClose h

processRequest :: RawRequest -> ErrorT String IO HttpResponse
processRequest rr@(RawRequest xs) = do
    unless (canParseUri rr) $ throwError "Bad Request"

    let method:_pathRaw:_ = words $ head xs
    let pathRaw = fixPath _pathRaw

    unless (method == "GET") $ throwError "Unrecognized Method"

    liftIO $ putStrLn $ "GET " ++ pathRaw

    when (".." `isInfixOf` pathRaw) $ throwError "Scary path!"
    isDir  <- liftIO $ D.doesDirectoryExist pathRaw
    isFile <- liftIO $ D.doesFileExist pathRaw
    unless (isDir || isFile) $ throwError "No such file!"

    let handler = if isDir then mkDirListResponse else mkFileResponse
    liftIO $ handler pathRaw

mkDirListResponse :: FilePath -> IO HttpResponse
mkDirListResponse path = do
    contentList <- concatMap pathToLink <$>
                   sort <$>
                   D.getDirectoryContents path
    return $ HttpResponse httpOk htmlMimeType $ B.pack ("<html><pre>" ++
                                                          contentList ++
                                                        "</pre></html>")
  where
    pathToLink e =
        "<a href=\"/" ++ path ++ "/" ++ e ++ "\">" ++  e ++ "</a>\n"

mkFileResponse :: FilePath -> IO HttpResponse
mkFileResponse path =
    withFile path ReadMode $ \h -> do
        data_ <- B.hGetContents h
        _ <- evaluate (B.length data_) -- Argh...
        let mimeType = defaultMimeLookup (T.pack path)
        return $ HttpResponse httpOk mimeType data_

sendResponse :: Handle -> HttpResponse -> IO ()
sendResponse h (HttpResponse statusCode mimeType body) = do
    hPutStrLn   h $ "HTTP/1.0 " ++ show statusCode
    B.hPutStrLn h $ (B.pack "Content-Type: ") `B.append` mimeType
    hPutStrLn   h   ""
    B.hPutStrLn h   body

canParseUri :: RawRequest -> Bool
canParseUri (RawRequest xs) = xs /= [] && length (words $ head xs) >= 2

fixPath :: String -> String
fixPath s = reverse $ dropWhile (=='/') $ reverse $ "." ++ s

htmlMimeType :: MimeType
htmlMimeType = defaultMimeLookup (T.pack "foo.html")




data HttpResponse = HttpResponse HttpStatusCode ByteString ByteString

data HttpStatusCode  = HttpStatusCode Int String
instance Show HttpStatusCode where
    show (HttpStatusCode number name) = show number ++ " " ++ name

httpOk, httpBadRequest :: HttpStatusCode
httpOk                  = HttpStatusCode 200 "OK"
httpBadRequest          = HttpStatusCode 400 "Bad Request"

newtype RawRequest = RawRequest [String]

data ServerConfig = ServerConfig { serverPortId :: N.PortID }




doOrDie :: IO a -> String -> IO a
doOrDie action explPrefix =
    catch action
          (\ex -> do
              let exs = show (ex :: IOException)
              hPutStr stderr (explPrefix ++ ".  " ++ exs)
              exitWith $ ExitFailure 1)

lDoOrDie :: MonadIO m => IO a -> String -> m a
lDoOrDie a e = liftIO $ doOrDie a e

untilM :: Monad m => m Bool -> m a -> m ()
untilM p a = p >>= \c -> unless c $ a >> untilM p a
