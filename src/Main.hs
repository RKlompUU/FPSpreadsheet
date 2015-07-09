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

import Sheet

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    f         <- frame [text := "Spreadsheet"]
    input1    <- entry f []
    input2    <- entry f []
    output    <- staticText f []

    sheet <- visibleCells f

    let cSpacing = 0 -- cell spacing (between columns and rows)

    set f [layout := margin 0
                   $ column cSpacing
                   $ map (\rowI -> row cSpacing
                                 $ map (widget . snd) (ins sheet !! rowI)
                         ) [0..length (ins sheet)-1]
                     ++ [widget output]
          ]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

        binput1  <- behaviorText input1 ""
        binput2  <- behaviorText input2 ""

        let
            result :: Behavior t (Maybe Int)
            result = f <$> binput1 <*> binput2
                where
                f x y = liftA2 (+) (readNumber x) (readNumber y)

            readNumber s = listToMaybe [x | (x,"") <- reads s]
            showNumber   = maybe "--" show

        sink output [text :== showNumber <$> result]

    network <- compile networkDescription
    actuate network
