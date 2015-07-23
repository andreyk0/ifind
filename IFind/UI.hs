{-# LANGUAGE OverloadedStrings #-}

module IFind.UI (
  runUI
) where

import Control.Applicative
import Data.Either
import Data.IORef
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import System.Exit (exitSuccess)
import Text.Printf

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Text.Regex.PCRE as RE
import qualified Text.Regex.PCRE.String as RES

import IFind.Config
import IFind.FS
import IFind.Opts
import Util.List


-- This type isn't pretty, but we have to specify the type of the
-- complete interface.  Initially you can let the compiler tell you
-- what it is.
type T = (Box (Box (HFixed FormattedText) (VFixed Edit))
              (List T.Text FormattedText))

data SearchApp =
  SearchApp {  -- widgets
              uiWidget :: Widget T
            , statusWidget :: Widget FormattedText
            , editSearchWidget :: Widget Edit
            , searchResultsWidget :: Widget (List T.Text FormattedText)
            , activateHandlers :: Handlers SearchApp
            -- search state
            , matchingFilePaths:: IORef [TextFilePath]
            , allFilePaths:: [TextFilePath]
            , ignoreCase:: IORef Bool
            }

focusedItemAttr:: IFindConfig -> Attr
focusedItemAttr conf = fgc `on` bgc
  where
    bgc = focusedItemBackgroundColor . uiColors $ conf
    fgc = focusedItemForegroundColor . uiColors $ conf

searchCountAttr:: IFindConfig -> Attr
searchCountAttr conf = fgc `on` bgc
  where
    bgc = searchCountBackgroundColor . uiColors $ conf
    fgc = searchCountForegroundColor . uiColors $ conf

-- | runs UI, returns matching file paths
runUI :: IFindOpts -> IFindConfig -> IO [TextFilePath]
runUI opts conf = do
  (ui, fg) <- newSearchApp opts conf

  c <- newCollection
  _ <- addToCollection c (uiWidget ui) fg

  runUi c $ defaultContext { focusAttr = focusedItemAttr conf }
  readIORef $ matchingFilePaths ui


newSearchApp :: IFindOpts -> IFindConfig -> IO (SearchApp, Widget FocusGroup)
newSearchApp opts conf = do
  editSearchWidget' <- editWidget
  searchResultsWidget' <- newTextList [] 1
  statusWidget' <- plainText "*>" >>= withNormalAttribute (searchCountAttr conf)
  activateHandlers' <- newHandlers

  _ <- setEditText editSearchWidget' $ T.pack (searchRe opts)

  uiWidget'  <- ( (hFixed 8  statusWidget') <++> (vFixed 1 editSearchWidget') )
             <-->
             (return searchResultsWidget')

  allFilePaths' <- findAllFilePaths opts conf
  matchingFilePathsRef <- newIORef allFilePaths'
  ignoreCaseRef <- newIORef $ caseInsensitive opts

  let sApp = SearchApp { uiWidget = uiWidget'
                       , statusWidget = statusWidget'
                       , editSearchWidget = editSearchWidget'
                       , searchResultsWidget = searchResultsWidget'
                       , activateHandlers = activateHandlers'
                       , matchingFilePaths = matchingFilePathsRef
                       , allFilePaths = allFilePaths'
                       , ignoreCase = ignoreCaseRef
                       }

  editSearchWidget' `onActivate` \_ -> do
    shutdownUi

  editSearchWidget' `onChange` \_ -> do
    updateSearchResults sApp
    return ()

  editSearchWidget' `onKeyPressed` \_ key mods -> do
    case (key, mods) of
      (KChar 'u', [MCtrl]) -> do
        ic <- readIORef ignoreCaseRef
        writeIORef ignoreCaseRef (not ic)
        updateSearchResults sApp
        return True
      (_, _)     ->
        return False

  searchResultsWidget' `onKeyPressed` \w key mods ->
    case (key, mods) of
      (KChar 'p', [MCtrl]) -> scrollUp w >> return True
      (KChar 'n', [MCtrl]) -> scrollDown w >> return True
      (KChar 'k', []) -> scrollUp w >> return True
      (KChar 'j', []) -> scrollDown w >> return True
      (_, _)           -> return False

  searchResultsWidget' `onItemActivated` \_ -> do
    selectedItem <- getSelected searchResultsWidget'
    case selectedItem of
      Just (_, (t, _)) -> do
        writeIORef matchingFilePathsRef [t]
        shutdownUi
      Nothing ->
        return ()

  fg <- newFocusGroup

  fg `onKeyPressed` \_ key _ -> do
    case key of
      KEsc -> do
        shutdownUi
        exitSuccess
      _    -> return False

  _ <- addToFocusGroup fg editSearchWidget'
  _ <- addToFocusGroup fg searchResultsWidget'

  _ <- updateSearchResults sApp

  return (sApp, fg)


updateSearchResults:: SearchApp -> IO ()
updateSearchResults sApp = do
  clearList $ searchResultsWidget sApp
  searchEditTxt <- getEditText $ editSearchWidget sApp
  ignoreCase' <- readIORef $ ignoreCase sApp

  stfp <- searchTxtToFilterPredicate ignoreCase' searchEditTxt
  case stfp  of
    Left es -> do
      writeIORef (matchingFilePaths sApp) []
      addToResultsList sApp $ fmap (T.pack) es

    Right filterPredicate -> do
      let matchingFps = filter (filterPredicate) $ allFilePaths sApp


      writeIORef (matchingFilePaths sApp) matchingFps
      addToResultsList sApp $ take 64 matchingFps

  updateStatusText sApp


addToResultsList:: SearchApp -> [T.Text] -> IO ()
addToResultsList sApp xs =
  mapM_ (\x -> do
          xw <- textWidget nullFormatter x
          addToList (searchResultsWidget sApp) x xw) xs


updateStatusText:: SearchApp -> IO ()
updateStatusText sApp = do
  matchingFps <- readIORef (matchingFilePaths sApp)
  let numResults = length matchingFps
  searchEditTxt <- getEditText $ editSearchWidget sApp
  ignoreCase' <- readIORef $ ignoreCase sApp
  let statusChr = if ignoreCase' then '*' else ']'

  if T.null searchEditTxt
    then setText (statusWidget sApp) $ T.pack $ "Search: "
    else setText (statusWidget sApp) $ T.pack $ printf "[%5d%c " numResults statusChr


-- | searchTxt can be of the form "foo!bar!baz",
--   this behaves similar to 'grep foo | grep -v bar | grep -v baz'
--   Return either Left regex compilation errors or Right file path testing predicate
searchTxtToFilterPredicate:: Bool -> T.Text -> IO (Either [String] (TextFilePath -> Bool))
searchTxtToFilterPredicate reIgnoreCase searchEditTxt =
  if T.null searchEditTxt
    then return $ Right (\_ -> True)
    else compileRegex

  where
    compileRegex = do cRes <- compileRes
                      case partitionEithers cRes
                        of ([], (includeRe:excludeRes)) ->
                             return $ Right $ \fp ->
                               let bsFp = TE.encodeUtf8 fp
                                in (RE.matchTest includeRe bsFp) &&
                                     (not . anyOf (fmap (RE.matchTest) excludeRes) $ bsFp)

                           (errs, _) -> return $ Left $ map (snd) errs

    mkRe:: T.Text -> IO (Either (RES.MatchOffset, String) RES.Regex)
    mkRe t = RES.compile reCompOpt reExecOpt (T.unpack t)

    reCompOpt =(if (reIgnoreCase) then RES.compCaseless else RES.compBlank)

    reExecOpt = RES.execBlank

    compileRes:: IO [Either (RES.MatchOffset, String) RE.Regex]
    compileRes = mapM (mkRe) $ T.splitOn "!" searchEditTxt
