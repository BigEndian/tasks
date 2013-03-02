module Tasks.Task (Task, task) where

import qualified Data.ByteString as BW
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Text.Read as R
import Data.Word(Word8(..))
import Data.Binary
import Data.Char(chr, ord)

data Task = Task { taskTitle :: B.ByteString, taskNotes :: Maybe B.ByteString, taskPriority :: Int }

task :: String -> Maybe String -> Task
task t ns = Task { taskTitle = B.pack t, taskNotes = tnsbs, taskPriority = 0 } 
               where
                  tnsbs = if isJust ns then (Just . B.pack . fromJust) ns
                          else Nothing :: Maybe B.ByteString

instance Show Task where
   show t = let f = B.unpack $ taskTitle t in 
               case (taskNotes t) of
                                    (Just nbs) -> f ++ " Notes: " ++ (B.unpack nbs)
                                    Nothing -> f

instance Read Task where
   readsPrec p s = [(Task { taskTitle = B.pack $ s, taskNotes = Just $ B.pack "", taskPriority = 0 }, s)]
   readListPrec = R.readListPrecDefault


instance Eq Task where
   (==) (Task t1t _ t1p) (Task t2t _ t2p) = and [t1t == t2t, t1p == t2p]


instance Ord Task where
   (<=) (Task _ _ t1p) (Task _ _ t2p) = (<=) t1p t2p



convertWord8ToChar :: Word8 -> Char
convertWord8ToChar w = chr . fromIntegral $ w

convertCharToWord8 :: Char -> Word8
convertCharToWord8 c = fromIntegral (ord c) :: Word8

putString :: String -> Put
putString str =
   case length str of
      0 -> fail "Invalid string passed to putString! String must be one character or longer"
      1 -> putWord8 w
      _ -> do putWord8 w; putString (drop 1 str)
   where
      c = let (fc:_) = str in fc
      w = convertCharToWord8 c

putTaskTitle :: Task -> Put
putTaskTitle (Task { taskTitle = tt }) = do
   putWord8 $ (fromIntegral (B.length tt) :: Word8)
   putString . B.unpack $ tt

putTaskNotes (Task { taskNotes = tn }) =
   if isNothing tn then
      putWord8 (0 :: Word8)
   else
      do
         putWord8 (fromIntegral (B.length $ fromJust tn) :: Word8)
         putString (B.unpack $ fromJust tn)

instance Binary Task where 
   put t@(Task { taskTitle = tt }) = do
      putTaskTitle t
      putTaskNotes t

   get = do ttlen <- getWord8 :: Get Word8
            tt <- readNWord8s [] ttlen
            tnlen <- getWord8 :: Get Word8
            tn <- readNWord8s [] tnlen
            if tnlen == 0 then
               return $ Task { taskTitle = tt, taskNotes = Nothing, taskPriority = 0 }
            else
               return $ Task { taskTitle = tt, taskNotes = Just tn, taskPriority = 0 }
            where
               readNWord8s :: (Num a, Eq a) => [Word8] -> a -> Get B.ByteString
               readNWord8s acc count =
                  if count == 0 then 
                     return $ B.pack $ map (\w -> chr (fromIntegral w)) acc
                  else
                     do
                        w <- get :: Get Word8
                        readNWord8s (acc ++ [w]) (count - 1)

exTask = Task { taskTitle = B.pack "Do the dishes", taskNotes = Just $ B.pack "Must be done by 10:00 today", taskPriority = 0 }
encTask = encode exTask
decTask = decode encTask :: Task
