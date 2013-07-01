import Prelude hiding (catch)
import qualified Network as N
import qualified System.Directory as D
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Exception (catch, IOException, evaluate)
import Control.Monad (liftM, forM_, forever, void, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Error (ErrorT, runErrorT, throwError, catchError)
import Data.List (isInfixOf, sort)
import System.Exit (exitWith, ExitCode(..))
import System.IO (Handle, hPutStr, hPutStrLn, hGetChar, hPutChar, hClose,
                  withFile, hIsEOF, hGetContents, hSetBuffering,
                  IOMode(ReadMode), BufferMode(..), stderr)
import System.Environment (getArgs)


type App = ReaderT ServerConfig IO


main :: IO ()
main = do
    args <- getArgs

    when (args /= []) $ do
        let dir = head args
        D.setCurrentDirectory dir
        putStrLn $ "Serving from directory '" ++ dir ++ "'..."

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
        (h, cHost, p) <- lDoOrDie (N.accept s) "Failed to accept client"
        liftIO $ putStrLn $ concat ["Accepted connection from ", show cHost,
                                    " on port ", show p, "."]
        handleClient h


handleClient :: Handle -> App ()
handleClient h = liftIO $ void $ forkIO $ do
    reqRaw <- lines <$> hGetContents h
    res <- runErrorT (processRequest reqRaw)
    case res of
        Right r -> sendResponse h r
        Left e  -> sendResponse h $ HttpResponse httpBadRequest e
    hClose h


processRequest :: [String] -> ErrorT String IO HttpResponse
processRequest reqRaw = do
    unless (canParseUri reqRaw) $ throwError "Bad Request"

    let method:_pathRaw:_ = words $ head reqRaw
    let pathRaw = "./" ++ _pathRaw

    unless (method == "GET") $ throwError "Unrecognized Method"

    liftIO $ putStrLn $ "GET " ++ pathRaw

    when (".." `isInfixOf` pathRaw) $ throwError "Scary path!"
    isDir  <- liftIO $ D.doesDirectoryExist pathRaw
    isFile <- liftIO $ D.doesFileExist pathRaw
    unless (isDir || isFile) $ throwError "No such file!"

    liftIO $ (if isDir then newDirResponse else newFileResponse) pathRaw


newDirResponse :: FilePath -> IO HttpResponse
newDirResponse path = do
    content <- D.getDirectoryContents path
    let contentList = concatMap formatEntry $ sort content
    return $ HttpResponse httpOk $ "<html><pre>" ++ contentList

  where
    formatEntry e = concat ["<a href=\"", path, "/", e, "\">", e, "</a>\n"]


newFileResponse :: FilePath -> IO HttpResponse
newFileResponse path =
    withFile path ReadMode $ \h -> do
        data_ <- hGetContents h
        evaluate (length data_) -- Argh...
        return $ HttpResponse httpOk data_


sendResponse :: Handle -> HttpResponse -> IO ()
sendResponse h (HttpResponse statusCode body) = do
    hPutStrLn h $ "HTTP/1.0 " ++ show statusCode
    when ("<html>" `isInfixOf` body)
         (hPutStrLn h "Content-Type: text/html; charset=UTF-8")
    hPutStrLn h ""
    hPutStrLn h body


canParseUri :: [String] -> Bool
canParseUri xs = xs /= [] && length (words $ head xs) >= 2


data HttpStatusCode  = HttpStatusCode Int String
instance Show HttpStatusCode where
    show (HttpStatusCode number name) = show number ++ " " ++ name
httpOk               = HttpStatusCode 200 "OK"
httpBadRequest       = HttpStatusCode 400 "Bad Request"
httpForbidden        = HttpStatusCode 403 "Forbidden"
httpNotFound         = HttpStatusCode 404 "Not found"
httpMethodNotAllowed = HttpStatusCode 405 "Method Not Allowed"

data HttpResponse = HttpResponse HttpStatusCode String deriving (Show)

data ServerConfig = ServerConfig { serverPortId :: N.PortID }


doOrDie :: IO a -> String -> IO a
doOrDie action explPrefix =
    catch action
          (\ex -> do
              let exs = show (ex :: IOException)
              hPutStr stderr (explPrefix ++ ".  " ++ exs)
              exitWith $ ExitFailure 1)
lDoOrDie a e = liftIO $ doOrDie a e


untilM :: Monad m => m Bool -> m a -> m ()
untilM p a = p >>= \c -> unless c $ a >> untilM p a
