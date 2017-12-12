{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where

import           Control.Exception (SomeException)
import qualified Control.Foldl as Fold
import           Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import           Control.Monad.Managed (MonadManaged)
import qualified Data.ByteString.Base64 as B64
import           Data.Either.Utils (fromEither)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Prelude hiding (FilePath)
import           Turtle
import qualified Turtle.Bytes as TB
import           Turtle.Shell (FoldShell(..))

-- | concatenation of Shell where the right operand is buffered into memory
--   first i.e., the stream from the left operand only gets added to the output
--   if the right operand succeeds to go through fully
bufferRight :: Shell a -> Shell a -> Shell a
bufferRight s1 s2 = Shell $ \(FoldShell step begin done) -> do
  ls <- fold s2 Fold.list
  x <- _foldShell s1 (FoldShell step begin return)
  _foldShell (select ls) (FoldShell step x done)

mkSegment
  :: FilePath   -- base path
  -> FilePath   -- relative path to file
  -> Shell Line
mkSegment base file =
  bufferRight (header Nothing) (input fullP) `catch`
    \(e :: SomeException) -> do
      eprintf
        ("WARNING: Cannot read in "%fp%" as text file\n"
          %w%"\n"
          %"...Falling back to binary input\n")
        file e
      bufferRight (header (Just "BASE64"))
        (unsafeTextToLine . T.decodeUtf8 . B64.encode <$> TB.input fullP)
  where
    fullP = base </> file
    header arg = pure . unsafeTextToLine $
      format ("{-# START_FILE "%s%fp%" #-}") (maybe "" (<>" ") arg) file


data OutputDest = OutputStdout | OutputFile FilePath
  deriving (Show, Eq)

data Config = Config
  { inputDir   :: FilePath
  -- if no outputFile
  , outputDest :: OutputDest
  } deriving (Show, Eq)

directOutput :: MonadIO io => OutputDest -> Shell Line -> io ()
directOutput OutputStdout = stdout
directOutput (OutputFile path) = output path

confParser :: Parser Config
confParser = Config
  <$> argDirPath      "input-dir"     "Input directory to make the hsfiles"
  <*> opt parseOutput "out"       'o' "Output file; '-' for stdout"
  where
    -- NOTE: use </> "" so that the path is forced into a dir path
    argDirPath o = fmap (</> "") . argPath o
    parseOutput "-" = Just OutputStdout
    parseOutput x = Just . OutputFile $ fromText x

-- | Find the relevant files under a given path
--   Yield output as relative paths.
--   Uses git to retrieve the list of files if possible
findFiles :: FilePath -> Shell FilePath
findFiles path = do
  hasGit <- testdir gitP
  if hasGit
    then fromText . lineToText <$> inproc "git"
         [ "--git-dir", pToT gitP, "--work-tree", pToT path
         , "ls-files", "--others", "--exclude-standard" ]
         empty
    else do
      f <- lstree path
      testfile f >>= guard
      Just relative <- pure $ stripPrefix path f
      return relative
  where
    gitP = path </> ".git"
    pToT = fromEither . toText

main :: IO ()
main = do
  conf@Config{..} <- options "hsfiles Generator" confParser
  printf ("Config: "%w%"\n") conf
  directOutput outputDest $ do
    file <- findFiles inputDir
    echo . unsafeTextToLine . fromEither . toText $ file
    mkSegment inputDir file
