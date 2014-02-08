import System.Directory (getAppUserDataDirectory
                        ,createDirectoryIfMissing
                        ,doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO
import Control.Monad
import Data.Maybe
import Text.Parsec
import Text.Parsec.ByteString

data Shorthand = Shorthand
    { keyS     :: String
    , valueS   :: String 
    , confirmS :: Bool
    } deriving (Show)

confDir :: IO FilePath
confDir = getAppUserDataDirectory "toolbox-haskell"

confFile :: IO FilePath
confFile = liftM (</> "toolbox.conf") confDir

touchConfigFile :: IO ()
touchConfigFile = do
    d <- confDir
    createDirectoryIfMissing True d
    exists <- confFile >>= doesFileExist  
    unless exists
        (confFile >>= \path -> openFile path AppendMode >>= hClose)

loadConfig :: IO [Shorthand]
loadConfig = do
    touchConfigFile
    fn <- confFile
    conf <- parseFromFile parseConf fn
    return $ either (const []) id conf

parseConf = liftM simplifyResult parseAll
    where
        toShortHand :: (String, Bool, String) -> Shorthand
        toShortHand (k,c,v) = Shorthand k v c

        simplifyResult :: [Maybe (String, Bool, String)] -> [Shorthand]
        simplifyResult = map toShortHand . catMaybes

        parseAll = many1 $ choice [parseRuleLine, parseCommentLine]
        parseRuleLine = do
            key <- many1 alphaNum
            confirm <- optionMaybe $ char '!'
            char ':'
            spaces
            value <- manyTill anyChar newline
            return $ Just (key, isJust confirm, value)
        parseCommentLine = do
            char '#'
            manyTill anyChar newline
            return Nothing

main = loadConfig >>= print
