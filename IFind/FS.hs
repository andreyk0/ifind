module IFind.FS (
  TextFilePath,
  findAllFilePaths
) where

import Control.Applicative
import System.FilePath.Find

import qualified Data.Text as T
import qualified Text.Regex.PCRE as RE
import qualified Text.Regex.PCRE.String as RES

import IFind.Config
import IFind.Opts
import Util.List

-- Convert Strings to Texts and work with that for incremental search
type TextFilePath = T.Text

findAllFilePaths:: IFindOpts -> IFindConfig -> IO [TextFilePath]
findAllFilePaths opts conf = do
  let filters = findFilters conf

  dirFilters <- mapM strToRegex $ excludeDirectories filters
  pathFilters <- mapM strToRegex $ excludePaths filters

  let dF = (filterOut dirFilters) <$> filePath
      fF = (filterOut pathFilters) <$> filePath
      regularFilesOrSymlinks = (fileType ==? RegularFile) ||? (fileType ==? SymbolicLink)

  (fmap (T.pack)) <$> find (dF) (fF &&? regularFilesOrSymlinks) (inDir opts)


-- | filter out file paths matching any of the given (as string) regexes
filterOut:: [RES.Regex] -> FilePath -> Bool
filterOut res fp = not $ anyOf toReMatchers fp
  where
    toReMatchers:: [FilePath -> Bool]
    toReMatchers = fmap (RE.matchTest) res

strToRegex:: String -> IO RES.Regex
strToRegex s = do
  rec <- RES.compile RE.blankCompOpt RE.blankExecOpt s
  case rec
    of Left (_, err) -> fail $ "Unable to compile [" ++ s ++ "] to regex: " ++ err
       Right r -> return r
