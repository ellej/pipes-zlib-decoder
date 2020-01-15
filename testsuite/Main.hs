
module Main where

import Pipes
import Pipes.Lift (runExceptP)
import qualified Pipes.Prelude as P

import Data.Functor.Identity (Identity, runIdentity)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Codec.Compression.Zlib as Z
import qualified Pipes.Decode.Zlib as PZ
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 1000) zlibTestGroup

instance Arbitrary CL.ByteString where
    arbitrary   = fmap CL.pack arbitrary

runTest :: BL.ByteString -> CL.ByteString
runTest text = BL.pack $ runIdentity $ P.toListM (each (BL.unpack text) >-> (() <$ runExceptP PZ.zlibDecode))

zlibQC :: CL.ByteString -> Bool
zlibQC s = runTest (Z.compress s) == s

zlibQCAlt :: CL.ByteString -> Bool
zlibQCAlt s = runTest (Z.compressWith zOpts s) == s where
   zOpts = Z.defaultCompressParams {
      Z.compressLevel = Z.bestCompression,
      Z.compressStrategy = Z.huffmanOnlyStrategy
   }

zlibHU :: CL.ByteString -> Assertion
zlibHU s = assertEqual "" s (runTest (Z.compress s))

zlibTestGroup :: TestTree
zlibTestGroup = testGroup "zlib" [
      testProperty "random, default zlib" zlibQC,
      testProperty "random, huffmanOnly zlib" zlibQCAlt,
      testCase "static, very long, replicate" (zlibHU (CL.replicate 1000000 'a'))
   ]
