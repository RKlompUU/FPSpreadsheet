module Src.Spreadsheet.IOSheet where

import Src.Spreadsheet.SheetType
import Src.Spreadsheet.SheetParser

writeSheet :: Sheet -> IO ()
writeSheet sh
  = do
  let str = undefined
  writeFile "~/.HsSheet/save" str

readSheet :: IO Sheet
readSheet
  = do
  str <- readFile "~/.HsSheet/save"
  return $ parseSheet str
