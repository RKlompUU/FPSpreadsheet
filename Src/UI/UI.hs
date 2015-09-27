module Src.UI.UI where

import Src.Spreadsheet.Sheet

import Src.UI.UITypes
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import Control.Monad
import Control.Concurrent.STM
import qualified Text.Blaze.Html as HTML
import qualified Text.Blaze.Html.Renderer.String as HTML
import qualified Data.Map as Map

import Debug.Trace

getHtml :: JSFunction String
getHtml = ffi "document.documentElement.innerHTML"

-- | Focus an element.
isFocused :: Element -> UI Bool
isFocused elm
  = do
  retVal <- callFunction $ ffi "$(%1).is(':focus')" elm
  return $ retVal == "true"


setup :: Window -> UI ()
setup rootWindow
  = do
  sheet <- initSheet

  ctxSh <- liftIO $ atomically $ newTVar sheet
  offsetSheet ctxSh (0,0)

  debugField <- UI.paragraph # set UI.text "Test"
  return rootWindow # set UI.title "Hello World!"

  rootWindowBody <- UI.getBody rootWindow
  on UI.keydown rootWindowBody (toKeyCodeM (rootKeyHandler ctxSh debugField))

  mapM_ (\cell -> on UI.keydown (grabCell cell) (toKeyCodeM $ sheetMod ctxSh rootWindow debugField cell))
        (concat $ sheetIns sheet)

  mapM_ (\cell -> on UI.keydown (grabShell cell) (toKeyCodeM $ shellKeyHandler debugField ctxSh cell))
        (concat $ sheetIns sheet)


  getBody rootWindow #+
    [  grid $ (:) (UI.body : map element (sheetColNs sheet))
            $ map (\(shRowNr, shRow) -> element shRowNr : shRow)
            $ zip (sheetRowNs sheet)
            $ (map . map)
              (\(_,(shell,cell)) -> element shell #+ [element cell])
              (sheetIns sheet)
    , element debugField ]

  UI.setFocus (grabShell $ (head . head) (sheetIns sheet))

  return ()


printText :: TVar Sheet -> Pos -> UI ()
printText ctxSh cPos
  = do
  sh <- getSheet ctxSh
  let inPos = cPos `posSubtr` sheetOffset sh
  case isInBox inPos (sheetInSize sh) of
    Nothing -> do
      case Map.lookup cPos (sheetCells sh) of
        Just c -> element (getSheetIn inPos sh) # set UI.value (show (Src.Spreadsheet.Sheet.text c)) >> return ()
        _ -> return ()
    _ -> return ()

printEval :: TVar Sheet -> Pos -> UI ()
printEval ctxSh cPos
  = do
  sh <- trace ("Printing eval pos: " ++ show cPos) getSheet ctxSh
  let inPos = cPos `posSubtr` sheetOffset sh
  case trace ("sheetOffset: " ++ show (sheetOffset sh)) isInBox inPos (sheetInSize sh) of
    Nothing -> do
      case Map.lookup cPos (sheetCells sh) >>= lExpr of
        Just e -> element (getSheetIn inPos sh) # set UI.value (show e) >> return ()
        _ -> trace "No expr!" return ()
    _ -> trace "not inside!" return ()

-- sheet modification
sheetMod :: TVar Sheet -> Window -> Element -> (Pos, (Element,Element)) -> KeyCode -> UI ()
--sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEnter
  = do
  -- Save edited content in the spreadsheet, and exit input focus
  sh <- getSheet ctxSh
  cCnt <- get UI.value (getSheetIn inPos sh)
  cPos <- getAbsoluteCPos ctxSh inPos
  let sh' = cellMod cCnt cPos sh

  liftIO $ atomically $ writeTVar ctxSh sh'
  mapM (\cPos -> printEval ctxSh cPos) (Map.keys . grabUpdatedCells $ sheetCells sh')
  let sh'' = sh' { sheetCells = resetUpdateFields (sheetCells sh')}
  liftIO $ atomically $ writeTVar ctxSh sh''

--  UI.setFocus inShell
--  element debugField # set UI.text (show (sheetOffset sh))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEsc
  = do
  sh <- getSheet ctxSh
  cPos <- getAbsoluteCPos ctxSh inPos
  cell2In (sheetCells sh) cPos inCell
  printEval ctxSh cPos
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) k
  = do
  element debugField # set UI.text (show k)
  --dumpHtml rootWindow debugField
  return ()

shellKeyHandler :: Element -> TVar Sheet -> (Pos, (Element,Element)) -> KeyCode -> UI ()
shellKeyHandler _ ctxSh (cPos, (cShell,cCell)) KeyCodeEnter
  = do
  cellHasFocus <- isFocused cCell
  UI.setFocus (if cellHasFocus then cShell else cCell)
shellKeyHandler _ ctxSh (cPos, (cShell,cCell)) KeyCodeEsc = UI.setFocus cShell
shellKeyHandler debugField ctxSh (cPos, (cShell,cCell)) k
  | k `elem` [KeyCodeUp, KeyCodeDown, KeyCodeLeft, KeyCodeRight]
      = do
      cellHasFocus <- isFocused cCell
      unless cellHasFocus $ moveFocus ctxSh (key2Dir k)
  | otherwise
      = element debugField # set UI.text (show k) >> return ()

rootKeyHandler :: TVar Sheet -> Element -> KeyCode -> UI ()
rootKeyHandler ctxSh debugField _ = return ()

dumpHtml :: Window -> Element -> UI ()
dumpHtml rootWindow debugField
  = do
  htmlCode <- callFunction getHtml
  -- This prettyfi stuff isn't actually working :/
  let prettyHtmlCode = HTML.renderHtml
                     $ HTML.toHtml htmlCode
  element debugField # set UI.text "" -- htmlCode
  return ()
