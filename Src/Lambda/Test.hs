{-|
Module      : Lambda.Test
Description : test suite for the lambda calculus expressions lexer and parser
Stability   : experimental
-}
module Lambda.Test where

import Lambda.ExprParser
import Lambda.Lambda


lambdaTests :: [(String, LC String -> IO ())]
lambdaTests = [ (("pLam"), testLambdaParser)
              , (("nfLam"), testNFLam) ]

testLambdaParser :: LC String -> IO ()
testLambdaParser = print

testNFLam :: LC String -> IO ()
testNFLam = putStrLn . show . nf . toIdInt
