{-# LANGUAGE OverloadedStrings #-}

module Basics where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Base16     as Hex
import qualified Data.Bits                  as Bits
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map.Strict            as Map

import Data.ByteString     (ByteString)
import Data.Char           (toLower)            
import Data.Function       (on)
import Data.List           (maximumBy)
import Data.List.Split     (chunksOf)
import Data.Word           (Word8)



decodeHex :: ByteString -> ByteString
decodeHex = fst . Hex.decode

xorB :: ByteString -> ByteString -> ByteString
xorB a b = BS.pack $ BS.zipWith Bits.xor a b

repeatingXor :: ByteString -> ByteString -> ByteString
repeatingXor input key  
  | BS.length key > 0 = xorB input extkey 
  | otherwise = ""
       where 
         extkey = BSL.toStrict $ BSL.take inputlength $ BSL.cycle $ BSL.fromStrict key
         inputlength = fromIntegral $ BS.length input      


chunks :: Int -> ByteString -> [ByteString]
chunks size s =  map BS.pack $ chunksOf size $ BS.unpack s  

fromByte :: Int -> Word8 -> ByteString
fromByte len ch = BS.replicate len ch


                         
freqmap :: Double -> Char -> Double
freqmap acc c = acc + Map.findWithDefault 0 c freq
  where freq = Map.fromList [('a', 8.167), ('b', 1.492), ('c', 2.782), ('d', 4.253), ('e', 12.70),
                         ('f', 2.228), ('g', 2.015), ('h', 6.094), ('i', 6.966), ('j', 0.153),
                         ('k', 0.772), ('l', 4.025), ('m', 2.406), ('n', 6.749), ('o', 7.507),
                         ('p', 1.929), ('q', 0.095), ('r', 5.987), ('s', 6.327), ('t', 9.056),
                         ('u', 2.758), ('v', 0.978), ('w', 2.360), ('x', 0.150), ('y', 1.974),
                         ('z', 0.074), (' ', 18.10)]
                         

weight :: ByteString -> Double
weight input = foldl freqmap 0.0 $ unpack input
    where unpack = map toLower . C8.unpack 


tryAllKeys :: ByteString -> (ByteString -> Double) -> [( Double , ByteString )]
tryAllKeys input scorefunc = [  (scorefunc (repeatingXor input $ BS.singleton key), BS.singleton key)  | key <- [1..255] ]

bestKey :: ByteString -> (ByteString -> Double) -> (Double, ByteString)
bestKey input scorefunc = maximumBy (on compare fst) $ tryAllKeys input scorefunc


