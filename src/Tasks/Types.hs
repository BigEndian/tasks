{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Tasks.Serialization(
   convertWord8ToChar,
   convertCharToWord8,
   
   stringToWByteString,
   wByteStringToCByteString,
   cByteStringToWByteString)
   where

{-
 - Basic methods to help me parse and store Tasks
 -}

import qualified Data.ByteString as BW
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.Binary -- for put
import Data.Word(Word8(..))
import Data.Char(chr, ord)
import Data.Bits

convertWord8ToChar :: Word8 -> Char
convertWord8ToChar = chr . fromIntegral

convertCharToWord8 :: Char -> Word8
convertCharToWord8 = fromIntegral . ord

stringToWByteString :: String -> BW.ByteString
stringToWByteString = BW.pack . map convertCharToWord8

wByteStringToString :: BW.ByteString -> String
wByteStringToString = map convertWord8ToChar . BW.unpack

wByteStringToCByteString :: BW.ByteString -> BC.ByteString
wByteStringToCByteString =
   BC.pack . wByteStringToString

cByteStringToWByteString :: BC.ByteString -> BW.ByteString
cByteStringToWByteString =
   BW.pack . (map convertCharToWord8) . BC.unpack

