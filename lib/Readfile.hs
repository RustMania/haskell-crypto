
module Readfile where
  

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding       as DTE

import    qualified Data.ByteString        as BS
import    qualified Data.ByteString.Base64     as B64

import Basics (decodeHex)

readCipher name  = do
  lines <- Text.readFile name
  return (  B64.decodeLenient $ DTE.encodeUtf8 lines )


readCiphers name  = do
  lines <- fmap Text.lines ( Text.readFile name )
  return ( fmap ( B64.decodeLenient . DTE.encodeUtf8) lines )
  
readHexCipher name  = do
  lines <- Text.readFile name
  return (  decodeHex $ DTE.encodeUtf8 lines )


readHexCiphers name  = do
  lines <- fmap Text.lines ( Text.readFile name )
  return ( fmap ( decodeHex . DTE.encodeUtf8) lines )
      
