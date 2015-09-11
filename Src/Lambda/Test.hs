module Src.Lambda.Test where

import Src.Lambda.ExprParser
import Src.Lambda.IdInt
import Src.Lambda.Lambda


lambdaTests :: [(String, LC IdInt -> IO ())]
lambdaTests = [ (("pLam"), testLambdaParser)
              , (("nfLam"), testNFLam) ]

testLambdaParser :: LC IdInt -> IO ()
testLambdaParser = print . show

testNFLam :: LC IdInt -> IO ()
testNFLam = print . nf
