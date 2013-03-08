-- |
-- Module: Tasks.Types
--
-- This module contains types relevant to the Tasks module,
-- but currently only exports TaskString and related functions,
-- useful for custom implementations of Binary.

module Tasks.Types
   (
     TaskString
   , tsString
   , tsToString
   , tsEmpty
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

-- | A custom type wrapping around Data.ByteString in order
-- to implement a custom instance of the Binary typeclass
newtype TaskString = TaskString BW.ByteString deriving (Read, Show, Eq)

-- | Create a TaskString given a String
tsString :: String -> TaskString
tsString = TaskString . stringToWByteString

-- | Convert a TaskString to a String
tsToString :: TaskString -> String
tsToString (TaskString bws) = wByteStringToString bws

-- | Check whether a given TaskString is empty
tsEmpty :: TaskString -> Bool
tsEmpty (TaskString bws) = bws == BW.empty

word8sToTaskString :: [Word8] -> TaskString
word8sToTaskString = TaskString . BW.pack

instance Binary TaskString where
   get = do
            (return . word8sToTaskString . init) =<< readWord8sUntil 0
         where
            readWord8sUntil :: Word8 -> Get [Word8]
            readWord8sUntil val = do
               w8 <- getWord8
               if w8 == val then
                  return $ [w8]
               else
                  (return . (w8:)) =<< (readWord8sUntil val)

   put (TaskString bws) = mapM_ putWord8 $ (BW.unpack bws) ++ [0]
