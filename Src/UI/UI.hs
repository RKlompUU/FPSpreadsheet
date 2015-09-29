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

import Src.Lambda.Lambda

import Debug.Trace

getHtml :: JSFunction String
getHtml = ffi "document.documentElement.innerHTML"

-- | Focus an element.
isFocused :: Element -> UI Bool
isFocused elm
  = do
  retVal <- callFunction $ ffi "$(%1).is(':focus')" elm
  return $ retVal == "true"

initUISheet :: UI UISheet
initUISheet = do
  let cols = 7
      rows = 12
      baseSheet = initSheet
  cells <- replicateM (rows*cols) (UI.input # set UI.size "7")
  shells <- replicateM (rows*cols) UI.button
  rowNrs <- replicateM rows (UI.body # set UI.text "0")
  colNrs <- replicateM cols (UI.body # set UI.text "0")
  let shelledCells = zip shells cells
      cells' = map (\(rI,r) -> zip [(rI,cI) | cI <- [0..cols]] r)
             $ zip [0..rows]
             $ subLists cols shelledCells
  return $ UISheet baseSheet (0,0) (0,0) cells' colNrs rowNrs


setup :: Window -> UI ()
setup rootWindow
  = do
  sheet <- initUISheet

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

focusSheetInShell :: TVar UISheet -> UI ()
focusSheetInShell ctxSh =
  do
    sh <- liftIO $ atomically $ readTVar ctxSh
    let focusIn = sheetCursor sh
    UI.setFocus (grabShell (sheetIns sh !! fst focusIn !! snd focusIn))

moveFocus :: TVar UISheet -> Pos -> UI ()
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

getAbsoluteCPos :: TVar UISheet -> Pos -> UI Pos
getAbsoluteCPos ctxSh relativePos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  return $ relativePos `posAdd` sheetOffset sh

-- Unsafe operation, will crash if an invalid position is given
getSheetIn :: Pos -> UISheet -> Element
getSheetIn (r,c) sh
  = grabCell $ sheetIns sh !! r !! c

grabShell :: (Pos, (Element, Element)) -> Element
grabShell = fst . snd
grabCell :: (Pos, (Element, Element)) -> Element
grabCell = snd . snd
grabPos :: (Pos, (Element, Element)) -> Pos
grabPos = fst

getUISheet :: TVar UISheet -> UI UISheet
getUISheet ctxSh = liftIO $ atomically $ readTVar ctxSh

cells2Ins :: TVar UISheet -> UI ()
cells2Ins ctxSh
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  mapM_ (\(p, (_,elm)) -> cell2In (sheetCells sh) (p `posAdd` sheetOffset sh) elm) (concat $ sheetIns sh)
  --liftIO $ atomically $ writeTVar ctxSh (sh {sheetIns = })

cell2In :: Sheet -> Pos -> Element -> UI ()
cell2In cs pos elm
  = do
  let (Cell text _) = Map.findWithDefault emptyCell pos cs
  oldVal <- get UI.value elm
  unless (text == oldVal) $ element elm # set UI.value text >> return ()

offsetSheet :: TVar UISheet -> Pos -> UI ()
offsetSheet ctxSh pos
  = do
  sh <- getUISheet ctxSh
  liftIO $ atomically $ writeTVar ctxSh (sh {sheetOffset = pos})
  mapM_ (\(elm,rowNr) -> element elm # set UI.text (show rowNr)) (zip (sheetRowNs sh) [fst pos..])
  mapM_ (\(elm,colNr) -> element elm # set UI.text (show colNr)) (zip (sheetColNs sh) [snd pos..])
  cells2Ins ctxSh
  return ()

scrollSheet :: TVar UISheet -> Pos -> UI ()
scrollSheet ctxSh dPos
  = do
  sh <- liftIO $ atomically $ readTVar ctxSh
  offsetSheet ctxSh (dPos `posAdd` sheetOffset sh)

cellMod :: String -> Pos -> UISheet -> UISheet
cellMod cCnt cPos sh
  = let mC     = Map.lookup cPos (sheetCells sh)
        lExpr' = parseExpr cCnt >>= return . nf . toIdInt
        c'     = Cell cCnt lExpr'
    in sh { sheetCells = updateCell cPos c' (sheetCells sh) }




printText :: TVar UISheet -> Pos -> Pos -> UI ()
printText ctxSh inPos cOffset
  = do
  sh <- getUISheet ctxSh
  case Map.lookup (inPos `posAdd` cOffset) (sheetCells sh) of
    Just c -> element (getSheetIn inPos sh) # set UI.value (show (Src.Spreadsheet.Sheet.text c)) >> return ()
    _ -> return ()

printEval :: TVar UISheet -> Pos -> Pos -> UI ()
printEval ctxSh inPos cOffset
  = do
  sh <- getUISheet ctxSh
  let c' = Map.lookup (inPos `posAdd` cOffset) (sheetCells sh)
  case (c' >>= lExpr) of
    Just e -> element (getSheetIn inPos sh) # set UI.value (show e) >> return ()
    _ -> return ()

-- sheet modification
sheetMod :: TVar UISheet -> Window -> Element -> (Pos, (Element,Element)) -> KeyCode -> UI ()
--sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEnter
  = do
  -- Save edited content in the spreadsheet, and exit input focus
  sh <- getUISheet ctxSh
  cCnt <- get UI.value (getSheetIn inPos sh)
  cPos <- getAbsoluteCPos ctxSh inPos
  let sh' = cellMod cCnt cPos sh

  liftIO $ atomically $ writeTVar ctxSh sh'
  printEval ctxSh inPos (sheetOffset sh')

--  UI.setFocus inShell
--  element debugField # set UI.text (show (sheetOffset sh))
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) KeyCodeEsc
  = do
  sh <- getUISheet ctxSh
  cPos <- getAbsoluteCPos ctxSh inPos
  cell2In (sheetCells sh) cPos inCell
  printEval ctxSh inPos (sheetOffset sh)
sheetMod ctxSh rootWindow debugField (inPos,(inShell,inCell)) k
  = do
  element debugField # set UI.text (show k)
  --dumpHtml rootWindow debugField
  return ()

shellKeyHandler :: Element -> TVar UISheet -> (Pos, (Element,Element)) -> KeyCode -> UI ()
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

rootKeyHandler :: TVar UISheet -> Element -> KeyCode -> UI ()
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
