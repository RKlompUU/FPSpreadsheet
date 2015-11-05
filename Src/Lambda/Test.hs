{-|
Module      : Lambda.Test
Description : test suite for the lambda calculus expressions lexer and parser
Stability   : experimental
-}
module Src.Lambda.Test where

import Src.Lambda.ExprParser
import Src.Lambda.Lambda


lambdaTests :: [(String, LC String -> IO ())]
lambdaTests = [ (("pLam"), testLambdaParser)
              , (("nfLam"), testNFLam) ]

testLambdaParser :: LC String -> IO ()
testLambdaParser = print

testNFLam :: LC String -> IO ()
testNFLam = putStrLn . show . nf . toIdInt
