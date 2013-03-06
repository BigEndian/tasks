module Tasks.Serialization(
   convertWord8ToChar,
   convertCharToWord8,
   
   stringToWByteString,
   wByteStringToCByteString,
   cByteStringToWByteString,

   putString,
   putWByteString,
   putCByteString)
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
   BC.pack . (map convertWord8ToChar) . BW.unpack

cByteStringToWByteString :: BC.ByteString -> BW.ByteString
cByteStringToWByteString =
   BW.pack . (map convertCharToWord8) . BC.unpack

putString :: String -> Put
putString [] = 
   fail "Invalid string passed to putString! String must be one character or longer!"
putString str =
   case length str of
      1 -> putWord8 w
      _ -> do putWord8 w; putString (drop 1 str)
   where
      c = let (fc:_) = (take 1 str) in fc
      w = convertCharToWord8 c

putWByteString :: BW.ByteString -> Put
putWByteString wbs =
   case BW.length wbs of
      0 -> fail "Invalid string passed to putByteString! String must be one character or longer!"
      1 -> putWord8 w
      _ -> do putWord8 w; putWByteString (BW.drop 1 wbs)
   where
      w = let (fw:_) = BW.unpack $ BW.take 1 wbs in fw

putCByteString :: BC.ByteString -> Put
putCByteString cbs = putWByteString (cByteStringToWByteString cbs)
