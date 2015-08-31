{-----------------------------------------------------------------------------
    reactive-banana-wx

    Example: Very simple arithmetic
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

module Main where

import Data.Maybe

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

-- wx wrapper: heinrich apfelmus
-- sodium library?

import Src.Sheet


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    f         <- frame [text := "Spreadsheet"]
    input1    <- entry f []
    input2    <- entry f []
    output    <- staticText f []

    sheet <- initSheet f

    --itemAppend input1 (5 :: Int)

    let cSpacing = 0 -- cell spacing (between columns and rows)

    set f [layout := margin 0
                   $ column cSpacing
                   $ map (\rowI -> row cSpacing
                                 $ map (widget . snd) (sheetIns sheet !! rowI)
                         ) [0..length (sheetIns sheet)-1]
                     ++ [widget output]
          ]


    let bSheetAction :: Behavior t Sheet
        bSheetAction
          = accumB sheet (eCellChanged <$ never)
          where eCellChanged :: Sheet -> Sheet
                eCellChanged sh = undefined



    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

        binput1  <- behaviorText input1 ""
        binput2  <- behaviorText input2 ""

        let
            result :: Behavior t (Maybe Int)
            result = func <$> binput1 <*> binput2
                where
                func x y = liftA2 (+) (readNumber x) (readNumber y)

            readNumber s = listToMaybe [x | (x,"") <- reads s]
            showNumber   = maybe "--" show

        sink output [text :== showNumber <$> result]

    network <- compile networkDescription
    actuate network
