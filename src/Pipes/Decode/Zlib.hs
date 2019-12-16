
module Pipes.Decode.Zlib (zlibDecode) where

import Prelude hiding (foldr, mapM_)
import Control.Monad hiding (mapM_)
import Data.Bits (Bits, testBit, (.&.), setBit, clearBit, shiftL)
import Data.Foldable (foldr, mapM_)
import Data.List (mapAccumL)
import Data.Sequence (Seq, (|>))
import Data.Word (Word8, Word32)
import Pipes
import Pipes.Lift

import qualified Control.Monad.State.Strict as ST
import qualified Data.List as L
import qualified Data.Sequence as S

data ZState = ZState {
   getHistory :: !IOut,
   adlerS1 :: !Integer,
   adlerS2 :: !Integer,
   current :: ![Bool]
}

type IOut = Seq Word8
data Tree = Node Tree Tree | Leaf Word32 | Null
type Z r = forall m. Monad m => Pipe Word8 Word8 (ST.StateT ZState m) r

emptyZState :: ZState
emptyZState = ZState mempty 1 0 []

zAddBT :: Word8 -> ZState -> ZState
zAddBT w (ZState bt s1 s2 c) = ZState app s1' s2' c where
   app = S.drop (S.length bt - 32768) (bt |> w)
   s1' = (s1 + fromIntegral w) `mod` 65521
   s2' = (s2 + s1') `mod` 65521

oAdd :: Word8 -> Z ()
oAdd el = yield el >> stMod (zAddBT el)

stGet :: Z ZState
stGet = ST.get

stMod :: (ZState -> ZState) -> Z ()
stMod = ST.modify

cPut :: [Bool] -> Z ()
cPut bs = stMod $ \st -> st { current = bs }

-- Data flow.

getByte :: Z Word8
getByte = cPut [] >> await

getBytes :: Int -> Z [Word8]
getBytes n = replicateM n getByte

getBits :: Int -> Z [Bool]
getBits n = replicateM n getBit

getBit :: Z Bool
getBit = current <$> stGet >>= \case
   [] -> getByte >>= cPut . toBits >> getBit
   b:bs -> b <$ cPut bs

-- Pipe to decode a zlib stream.
zlibDecode :: Monad m => Pipe Word8 Word8 m ()
zlibDecode = evalStateP emptyZState z where
   z = zlibHeader >> loop >> zlibAdler32
   loop = do
      lastBlock <- getBit
      infBlockType >>= infMethod
      unless lastBlock loop

-- Zlib header parser.
zlibHeader :: Z ()
zlibHeader = getBytes 2 >>= inner where
   inner [b1, b2]
      | b1 .&. 15 /= 8 = fail "unknown compression method"
      | testBit b2 5 = fail "dict not supported"
      | mod (asInt b1 * 256 + asInt b2) 31 /= 0 = fail "FCHECK is not 0"
      | otherwise = return ()
   inner _ = fail "zlibHeader: getBytes 2 did not return 2 bytes"

-- Adler32 checksum.
zlibAdler32 :: Z ()
zlibAdler32 = do
   c1 <- liftM2 ((+) . (*) 65536) adlerS2 adlerS1 <$> stGet
   c2 <- bytesToNumM <$> getBytes 4
   when (c1 /= c2) (fail $ "adler32 checksum failed (calc: "
      ++ show c1 ++ ", from data: " ++ show c2 ++ ")")


infBlockType :: Z Int
infBlockType = bitsToInt <$> getBits 2

-- Parse inflation method.
infMethod :: Int -> Z ()
infMethod i = case i of
   0 -> infNoCompression
   1 -> infFixedHuffman
   2 -> infDynamicHuffman
   a -> fail ("got invalid block type (" <> show a <> ")")

-- Inflate uncompressed stream.
infNoCompression :: Z ()
infNoCompression = do
   len <- bytesToIntL <$> getBytes 2
   _clen <- bytesToIntL <$> getBytes 2
   replicateM_ len (getByte >>= oAdd . fromIntegral)

fixedHuffTree :: (Tree, Tree)
fixedHuffTree = (ltree, dtree) where
   ltree = buildHuffTree (join
      [getCs 8 [0..143], getCs 9 [144..255], getCs 7 [256..279], getCs 8 [280..287]])
   dtree = buildHuffTree (getCs 5 [0..29])
   getCs l cs = (l,) <$> cs

-- Inflate with a fixed Fuffman tree.
infFixedHuffman :: Z ()
infFixedHuffman = infUseHuffTree fixedHuffTree

-- Inflate with a dynamic Huffman tree.
infDynamicHuffman :: Z ()
infDynamicHuffman = buildDynamicTree >>= infUseHuffTree

buildDynamicTree :: Z (Tree, Tree)
buildDynamicTree = do
   hlit <- bitsToInt <$> getBits 5
   hdist <- bitsToInt <$> getBits 5
   hclen <- (+4) . bitsToInt <$> getBits 4
   hclengths <- replicateM hclen (bitsToInt <$> getBits 3)
   let tree = buildHuffTree (L.zip hclengths [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15])
   distlit <- buildHuffCodes tree (hlit + hdist + 258)
   let lit = zip (take (hlit + 257) distlit) [0..]
       dist = zip (drop (hlit + 257) distlit) [0..]
   return (buildHuffTree lit, buildHuffTree dist)

buildHuffCodes :: Tree -> Int -> Z [Int]
buildHuffCodes tree = build 0 where
   build _ i | i < 0 = fail "got negative code count"
   build _ 0 = return []
   build prev i = walkTree tree >>= inner . fromIntegral where
      inner w
         | w >= 0 && w <= 15 = (w :) <$> build w (i - 1)
         | w == 16 = nextMult 3 2 >>= repVal prev
         | w == 17 = nextMult 3 3 >>= repVal 0
         | w == 18 = nextMult 11 7 >>= repVal 0
         | otherwise = fail "got illegal huffman alphabet code"
      nextMult offset bits = (+offset) . bitsToInt <$> getBits bits
      repVal val cnt = (replicate cnt val ++) <$> build val (i - cnt)

buildHuffTree :: [(Int, Word32)] -> Tree
buildHuffTree = fst . build 0 . cleanup where
   cleanup = L.sort . filter ((/= 0) . fst)
   build _ [] = (Null, [])
   build i orig@((l, c) : rest)
      | i == l = (Leaf c, rest)
      | i < l = let (zTree, zRest) = build (i + 1) orig
                    (oTree, oRest) = build (i + 1) zRest
                in (Node zTree oTree, oRest)
      | otherwise = error "buildHuffTree: this should never happen"

walkTree :: Tree -> Z Word32
walkTree = walk where
   walk Null = fail "got null tree"
   walk (Node l r) = getBit >>= walk . bool r l
   walk (Leaf w) = return w

-- Like 'maybe' but for Bool.
bool :: a -> a -> Bool -> a
bool a1 a2 b = if b then a1 else a2

-- left node = 0, right node = 1
infUseHuffTree :: (Tree, Tree) -> Z ()
infUseHuffTree (ltree, dtree) = continue where
   continue = walkTree ltree >>= inner
   inner w
      | w < 256 = oAdd (fromIntegral w) >> continue
      | w > 256 = infBackTrack dtree w >> continue
      | otherwise = return () -- end of block

-- Backtrack in the history.
infBackTrack :: Tree -> Word32 -> Z ()
infBackTrack dTree w = do
   bt <- getHistory <$> stGet
   bLen <- zLookup lengths w
   bDist <- walkTree dTree >>= zLookup dists
   if S.length bt == 0
      then fail "tried to backtrack, but found no history"
      else mapM_ oAdd (takeCycle bLen (S.drop (S.length bt - bDist) bt))

zLookup :: [(Word32, (Int, Int))] -> Word32 -> Z Int
zLookup list w = case L.lookup w list of
   Nothing -> fail "backtrack failed: could not find length or distance"
   Just (val, len) -> (val+) . bitsToInt <$> getBits len

-- Take n elements from a Seq, cycling the sequence if n is larger than the length of the Seq.
takeCycle :: Int -> Seq a -> Seq a
takeCycle i s
   | S.null s = s
   | i <= 0 = S.empty
   | otherwise = S.take i s <> takeCycle (i - S.length s) s

lengths :: [(Word32, (Int, Int))]
lengths = zip [257..285] (baseCodes 3 ((8,0) : map (4,) [1..5]) ++ [(258,0)])

dists :: [(Word32, (Int, Int))]
dists = zip [0..29] (baseCodes 1 list) where
   list = (4,0) : map (2,) [1..13]

baseCodes :: Int -> [(Int, Int)] -> [(Int, Int)]
baseCodes firstVal = snd . mapAccumL inner firstVal . list where
   inner acc len = (acc + (2^len), (acc, len))
   list = join . map (uncurry replicate)

-- Set or clear a bit.
putBit :: Bits b => b -> Int -> Bool -> b
putBit b i v = (if v then setBit else clearBit) b i

-- Convert words to an int. Least significant byte first.
bytesToIntL :: [Word8] -> Int
bytesToIntL = bytesToNumL

-- Convert words to a number. Least significant byte first.
bytesToNumL :: (Integral a, Bits a) => [Word8] -> a
bytesToNumL = foldr (\x r -> shiftL r 8 + fromIntegral x) 0

-- Convert words to a number. Most significant byte first.
bytesToNumM :: (Integral a, Bits a) => [Word8] -> a
bytesToNumM = bytesToNumL . reverse

-- Convert a list of booleans (bits) to an Int.
bitsToInt :: [Bool] -> Int
bitsToInt = foldr (\x r -> putBit (shiftL r 1) 0 x) 0

-- Convert something of the Bits class to a list of booleans.
toBits :: Bits b => b -> [Bool]
toBits b = testBit b <$> [0..7]

-- Helper to convert from an Integral to an Int.
asInt :: Integral a => a -> Int
asInt = fromIntegral
