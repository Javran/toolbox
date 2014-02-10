import System.Directory (getAppUserDataDirectory
                        ,createDirectoryIfMissing
                        ,doesFileExist)
import System.FilePath.Posix ((</>))
import qualified System.IO.Strict as S
import System.IO
import Control.Monad
import Data.Maybe
import Text.Parsec
import Text.Parsec.ByteString
import Data.Char
import Data.Either
import qualified Data.ByteString.Char8 as B

data Shorthand = Shorthand
    { keyS     :: String
    , valueS   :: String 
    , confirmS :: Bool
    } deriving (Show)

confDir :: IO FilePath
confDir = getAppUserDataDirectory "toolbox-haskell"

confFile :: IO FilePath
confFile = liftM (</> "toolbox.conf") confDir

touchConfigFile :: IO FilePath
touchConfigFile = do
    d <- confDir
    createDirectoryIfMissing True d
    exists <- confFile >>= doesFileExist  
    unless exists
        (confFile >>= (`openFile` AppendMode) >>= hClose)
    confFile     

loadConfig :: IO [Shorthand]
loadConfig = do
    fn <- touchConfigFile
    contents <- S.readFile fn
    let contentLines = lines contents
    let isCommentLine s =
            null s
         || all isSpace s
         || head s == '#'
    let confLinesB = map B.pack $ filter (not . isCommentLine) contentLines
    return $ rights $ map parseConfLine confLinesB

parseConfLine :: B.ByteString -> Either ParseError Shorthand
parseConfLine line = parse lineParser "StupidArgument" line
  where
    lineParser :: Parser Shorthand
    lineParser = do
            key <- many1 alphaNum
            confirm <- optionMaybe $ char '!'
            char ':'
            spaces
            value <- (many1 anyChar) >>= \a -> eof >> return a
            return $ Shorthand key value $ isJust confirm

main :: IO ()
main = loadConfig >>= print
