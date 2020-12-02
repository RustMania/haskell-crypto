{-# LANGUAGE OverloadedStrings #-}


module Symmetric where 
  

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..),nullIV)
import Crypto.Error (throwCryptoError)

import Data.List (nub, sortOn,concat)
import Data.Ord
import qualified Data.ByteString            as BS
import Data.ByteString     (ByteString)

import Readfile (readCipher, readHexCiphers)

import Basics ( chunks , xorB, fromByte )

  
read7Input :: IO BS.ByteString
read7Input = readCipher "7.txt"


read8input :: IO [BS.ByteString]
read8input = readHexCiphers "8.txt"

read10input :: IO BS.ByteString
read10input = readCipher "10.txt"

--Some content-encryption algorithms assume the input length is a
--   multiple of k octets, where k is greater than one.  For such
--   algorithms, the input shall be padded at the trailing end with
--   k-(lth mod k) octets all having value k-(lth mod k), where lth is
--   the length of the input.  In other words, the input is padded at
--   the trailing end with one of the following strings:
--
--                     01 -- if lth mod k = k-1
--                  02 02 -- if lth mod k = k-2
--                      .
--                      .
--                      .
--            k k ... k k -- if lth mod k = 0  <-- THIS MUST BE WRONG!!!
--
--   The padding can be removed unambiguously since all input is padded,
--   including input values that are already a multiple of the block size,
--   and no padding string is a suffix of another.  This padding method is
--   well defined if and only if k is less than 256.

--   if input is shorted than k , padding octet value is (k - length)
padTo :: Int -> ByteString -> ByteString
padTo k b = BS.concat [b, pad]  
  where lth = BS.length b 
        pad = BS.replicate (k - lth) $ fromIntegral (k - lth) 
        

--   pad to multiple of 16 
padTo16 :: ByteString -> ByteString
padTo16 b = padTo k b 
  where k =  (((BS.length b) - 1 ) `div` 16 + 1 ) * 16 
        

encryptEcb :: ByteString -> ByteString -> ByteString
encryptEcb key plainData = ecbEncrypt  ctx  plainData
  where ctx :: AES128
        ctx = throwCryptoError $ cipherInit key
        
decryptEcb :: ByteString -> ByteString -> ByteString
decryptEcb key plainData = ecbDecrypt ctx plainData
  where ctx :: AES128
        ctx = throwCryptoError $ cipherInit key


-- one stage of CBC
blockEncrypt key (output,iv) block =  let 
                                   input = xorB block iv 
                                   cipher = encryptEcb key input
                                  in ( BS.append output cipher , cipher )


-- implement CBC mode manually
encryptCbc :: ByteString -> ByteString -> ByteString -> ByteString
encryptCbc key iv plainData = fst $ foldl blockEncrypt (BS.empty,iv) blocks 
  where blockEncrypt (output,iv) block =  let 
                                   input = xorB block iv 
                                   cipher = encryptEcb key input
                                  in ( BS.append output cipher , cipher )
        blocks = chunks 16 (padTo16 plainData)                            
        
-- implement decrypt from CBC manually 
decryptCbc :: ByteString -> ByteString -> ByteString -> ByteString
decryptCbc key iv cypherText = fst $ foldl blockDecrypt (BS.empty,iv) blocks 
  where blockDecrypt (output,iv) block =  let
                                   plainData = decryptEcb key block   
                                   plainText = xorB plainData iv                                    
                                  in ( BS.append output plainText , block )
        blocks = chunks 16 cypherText 
        

repeatedBlocks :: ByteString -> Int
repeatedBlocks cipher = length blocks - (length $ nub blocks)
    where blocks = chunks 16 cipher 



