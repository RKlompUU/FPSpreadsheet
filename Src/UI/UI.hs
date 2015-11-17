module UI.UI where

import Spreadsheet.Sheet

import UI.UITypes
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI


import Control.Monad
import Control.Concurrent.STM
import qualified Text.Blaze.Html as HTML
import qualified Text.Blaze.Html.Renderer.String as HTML
import qualified Data.Map as Map

import Lambda.Lambda

import Debug.Trace


type SheetTy = Sheet (LC String)

getHtml :: UI.JSFunction String
getHtml = UI.ffi "document.documentElement.innerHTML"

-- | Focus an element.
isFocused :: UI.Element -> UI.UI Bool
isFocused elm
  = do
  retVal <- UI.callFunction $ UI.ffi "$(%1).is(':focus')" elm
  return $ retVal == "true"

initUISheet :: UI.UI UISheet
initUISheet = do
  let cols = 7
      rows = 12
      baseSheet = initSheet
  cells <- replicateM (rows*cols) (UI.input UI.# UI.set UI.size "14")
  shells <- replicateM (rows*cols) UI.button
  rowNrs <- replicateM rows (UI.body UI.# UI.set UI.text "0")
  colNrs <- replicateM cols (UI.body UI.# UI.set UI.text "0")
  let shelledCells = zip shells cells
      cells' = map (\(rI,r) -> zip [(rI,cI) | cI <- [0..cols]] r)
             $ zip [0..rows]
             $ subLists cols shelledCells
  return $ UISheet baseSheet (0,0) (0,0) cells' colNrs rowNrs


setup :: UI.Window -> UI.UI ()
setup rootWindow
  = do
  sheet <- initUISheet

  ctxSh <- liftIO $ atomically $ newTVar sheet
  offsetSheet ctxSh (0,0)

  debugField <- UI.paragraph UI.# UI.set UI.text "Test"
  return rootWindow UI.# UI.set UI.title "Hello World!"

  rootWindowBody <- UI.getBody rootWindow
  UI.on UI.keydown rootWindowBody (toKeyCodeM (rootKeyHandler ctxSh debugField))

  mapM_ (\cell -> UI.on UI.keydown (grabCell cell) (toKeyCodeM $ sheetMod ctxSh rootWindow debugField cell))
        (concat $ sheetIns sheet)

  mapM_ (\cell -> UI.on UI.keydown (grabShell cell) (toKeyCodeM $ shellKeyHandler debugField ctxSh cell))
        (concat $ sheetIns sheet)


  UI.getBody rootWindow UI.#+
    [  UI.grid $ (:) (UI.body : map UI.element (sheetColNs sheet))
            $ map (\(shRowNr, shRow) -> UI.element shRowNr : shRow)
            $ zip (sheetRowNs sheet)
            $ (map . map)
              (\(_,(shell,cell)) -> UI.element shell UI.#+ [UI.element cell])
              (sheetIns sheet)
    , UI.element debugField ]

  UI.setFocus (grabShell $ (head . head) (sheetIns sheet))

  return ()

focusSheetInShell :: TVar UISheet -> UI.UI ()
focusSheetInShell ctxSh =
  do
    sh <- liftIO $ atomically $ readTVar ctxSh
    let focusIn = sheetCursor sh
    UI.setFocus (grabShell (sheetIns sh !! fst focusIn !! snd focusIn))

moveFocus :: TVar UISheet -> Pos -> UI.UI ()
moveFocus ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  let (fRow,fCol) = dPos `posAdd` sheetCursor sh
      rows = length (sheetIns sh) - 1
      cols = length (head $ sheetIns sh) - 1
  case isInBox (fRow,fCol) ((0,0),(rows,cols)) of
    Just offset -> scrollSheet ctxSh offset
    Nothing -> liftIO (atomically $ writeTVar ctxSh (sh {sheetCursor = dPos `posAdd` sheetCursor sh}))
               >> focusSheetInShell ctxSh

getAbsoluteCPos :: TVar UISheet -> Pos -> UI.UI Pos
getAbsoluteCPos ctxSh relativePos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  return $ relativePos `posAdd` sheetOffset sh

-- Unsafe operation, will crash if an invalid position is given
getSheetIn :: Pos -> UISheet -> UI.Element
getSheetIn (r,c) sh
  = grabCell $ sheetIns sh !! r !! c

grabShell :: (Pos, (UI.Element, UI.Element)) -> UI.Element
grabShell = fst . snd
grabCell :: (Pos, (UI.Element, UI.Element)) -> UI.Element
grabCell = snd . snd
grabPos :: (Pos, (UI.Element, UI.Element)) -> Pos
grabPos = fst

getUISheet :: TVar UISheet -> UI.UI UISheet
getUISheet ctxSh = liftIO $ atomically $ readTVar ctxSh

cells2Ins :: TVar UISheet -> UI.UI ()
cells2Ins ctxSh
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  mapM_ (\(p, (_,elm)) -> cell2In (sheetCells sh) (p `posAdd` sheetOffset sh) elm) (concat $ sheetIns sh)
  --liftIO $ atomically $ writeTVar ctxSh (sh {sheetIns = })

cell2In :: SheetTy -> Pos -> UI.Element -> UI.UI ()
cell2In cs pos elm
  = do
  let (CellT text _ _) = Map.findWithDefault emptyCell pos cs
  oldVal <- UI.get UI.value elm
  unless (text == oldVal) $ UI.element elm UI.# UI.set UI.value text >> return ()

offsetSheet :: TVar UISheet -> Pos -> UI.UI ()
offsetSheet ctxSh pos
  = do
  sh <- getUISheet ctxSh
  liftIO $ atomically $ writeTVar ctxSh (sh {sheetOffset = pos})
  mapM_ (\(elm,rowNr) -> UI.element elm UI.# UI.set UI.text (show rowNr)) (zip (sheetRowNs sh) [fst pos..])
  mapM_ (\(elm,colNr) -> UI.element elm UI.# UI.set UI.text (show colNr)) (zip (sheetColNs sh) [snd pos..])
  cells2Ins ctxSh
  return ()

scrollSheet :: TVar UISheet -> Pos -> UI.UI ()
scrollSheet ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  offsetSheet ctxSh (dPos `posAdd` sheetOffset sh)

cellMod :: String -> Pos -> UISheet -> UISheet
cellMod cCnt cPos sh
  = let c'     = (getSheetCell cPos (sheetCells sh)) { Spreadsheet.Sheet.text = cCnt }
        cs'    = Map.insert cPos c' (sheetCells sh)
    in sh { sheetCells = snd $ runState updateEvals cs' }

uiSheetInSize :: UISheet -> (Pos,Pos)
uiSheetInSize sh =
  let rs = length $ sheetRowNs sh
      cs = length $ sheetColNs sh
  in ((0,0),(rs,cs))


printText :: TVar UISheet -> Pos -> UI.UI ()
printText ctxSh cPos
  = do
  sh <- getUISheet ctxSh
  let inPos = cPos `posSubtr` sheetOffset sh
  case isInBox inPos (uiSheetInSize sh) of
    Nothing -> do
      case Map.lookup cPos (sheetCells sh) of
        Just c -> UI.element (getSheetIn inPos sh) UI.# UI.set UI.value (Spreadsheet.Sheet.text c) >> return ()
        _ -> return ()
    _ -> return ()

printEval :: TVar UISheet -> Pos -> UI.UI ()
printEval ctxSh cPos
  = do
  sh <- getUISheet ctxSh
  let inPos = cPos `posSubtr` sheetOffset sh
  case isInBox inPos (uiSheetInSize sh) of
    Nothing -> do
      case Map.lookup cPos (sheetCells sh) >>= lExpr of
        Just e -> UI.element (getSheetIn inPos sh) UI.# UI.set UI.value (show e) >> return ()
        _ -> return ()
    _ -> return ()

-- sheet modification
sheetMod :: TVar UISheet -> UI.Window -> UI.Element -> (Pos, (UI.Element,UI.Element)) -> KeyCode -> UI.UI ()
--sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEnter
  = do
  -- Save edited content in the spreadsheet, and exit input focus
  sh <- trace "sheetMod" getUISheet ctxSh
  cCnt <- UI.get UI.value (getSheetIn inPos sh)
  cPos <- getAbsoluteCPos ctxSh inPos
  let sh' = cellMod cCnt cPos sh

  liftIO $ atomically $ writeTVar ctxSh sh'
  mapM (\cPos -> trace (show cPos) printEval ctxSh cPos) ((\t->trace (show t) t) . Map.keys . grabUpdatedCells $ sheetCells sh')
  let sh'' = sh' { sheetCells = resetUpdateFields (sheetCells sh')}
  liftIO $ atomically $ writeTVar ctxSh sh''

--  UI.setFocus inShell
--  element debugField # set UI.text (show (sheetOffset sh))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEsc
  = do
  sh <- getUISheet ctxSh
  cPos <- getAbsoluteCPos ctxSh inPos
  cell2In (sheetCells sh) cPos inCell
  printEval ctxSh cPos
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) k
  = do
  UI.element debugField UI.# UI.set UI.text (show k)
  --dumpHtml rootWindow debugField
  return ()

shellKeyHandler :: UI.Element -> TVar UISheet -> (Pos, (UI.Element,UI.Element)) -> KeyCode -> UI.UI ()
shellKeyHandler _ ctxSh (cPos, (cShell,cCell)) KeyCodeEnter
  = do
  cellHasFocus <- isFocused cCell
  UI.setFocus (if cellHasFocus then cShell else cCell)
  when (not cellHasFocus) $ printText ctxSh cPos
shellKeyHandler _ ctxSh (cPos, (cShell,cCell)) KeyCodeEsc = UI.setFocus cShell
shellKeyHandler debugField ctxSh (cPos, (cShell,cCell)) k
  | k `elem` [KeyCodeUp, KeyCodeDown, KeyCodeLeft, KeyCodeRight]
      = do
      cellHasFocus <- isFocused cCell
      unless cellHasFocus $ moveFocus ctxSh (key2Dir k)
  | otherwise
      = UI.element debugField UI.# UI.set UI.text (show k) >> return ()

rootKeyHandler :: TVar UISheet -> UI.Element -> KeyCode -> UI.UI ()
rootKeyHandler ctxSh debugField _ = return ()

dumpHtml :: UI.Window -> UI.Element -> UI.UI ()
dumpHtml rootWindow debugField
  = do
  htmlCode <- UI.callFunction getHtml
  -- This prettyfi stuff isn't actually working :/
  let prettyHtmlCode = HTML.renderHtml
                     $ HTML.toHtml htmlCode
  UI.element debugField UI.# UI.set UI.text "" -- htmlCode
  return ()
