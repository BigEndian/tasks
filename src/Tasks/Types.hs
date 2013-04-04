-- |
-- Module: Tasks.Types
--
-- This module contains convenience functions useful in 
-- handling and creating bytestrings in other parts of this package

module Tasks.Types
   (
     bs
   , bsToString
   , bsEmpty
   ) where

import qualified Data.ByteString as BW
import Data.Binary
import Data.Char(chr, ord)

convertWord8ToChar :: Word8 -> Char
convertWord8ToChar = chr . fromIntegral

convertCharToWord8 :: Char -> Word8
convertCharToWord8 = fromIntegral . ord

stringToWByteString :: String -> BW.ByteString
stringToWByteString = BW.pack . map convertCharToWord8

wByteStringToString :: BW.ByteString -> String
wByteStringToString = map convertWord8ToChar . BW.unpack

-- | Create a ByteString given a String
bs :: String -> BW.ByteString
bs = stringToWByteString

-- | Convert a ByteString to a String
bsToString :: BW.ByteString -> String
bsToString bws = wByteStringToString bws

-- | Check whether a given ByteString is empty
bsEmpty :: BW.ByteString -> Bool
bsEmpty = (==0) . length . BW.unpack
