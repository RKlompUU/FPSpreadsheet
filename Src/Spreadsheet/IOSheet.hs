{-|
Module      : SpreadSheet.IOSheet
Description : Saving and opening spreadsheets (from disk, or some other destination/source)
Stability   : unfinished functions
-}
module Spreadsheet.IOSheet where

import Spreadsheet.SheetType
import Spreadsheet.SheetParser

-- | Not implemented yet: Writes a spreadsheet to disk
writeSheet :: Sheet a -> IO ()
writeSheet sh
  = do
  let str = undefined
  writeFile "~/.HsSheet/save" str

-- | Not implemented yet: Retrieves a spreadsheet from disk
readSheet :: IO (Sheet a)
readSheet
  = do
  str <- readFile "~/.HsSheet/save"
  return $ parseSheet str
