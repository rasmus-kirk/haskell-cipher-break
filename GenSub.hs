module GenSub (
    getKeys,
    encrypt,
    decrypt,
    switchKey
) where

import qualified Data.Map as Map
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Util

type GenSubKey = [(Char, Char)]

getKeys :: String -> GenSubKey
getKeys cipher = 
  let a = (map fst $ dictToList (frequencyAnalysis cipher)) 
      b = (map fst englishFreq) in
    error "" 

--zipWithPadding :: a -> b -> [a] -> [b] -> [(a,b)]
--zipWithPadding a b (x:xs) (y:ys) = (x,y) : zipWithPadding a b xs ys
--zipWithPadding a _ []     ys     = zip (repeat a) ys
--zipWithPadding _ b xs     []     = zip xs (repeat b)switchKey :: (Char, Char) -> GenSubKey -> GenSubKey

switchKey (k1, v1) k = 
  let v2 = lookupCharOrError k1 k snd
      k2 = lookupCharOrError v1 k fst
    in changeChar (k2, v2) $ changeChar (k1, v1) k

decrypt :: String -> GenSubKey -> String
decrypt m k = map decryptChar m where
  decryptChar c = lookupCharOrError c k snd

encrypt :: String -> GenSubKey -> String
encrypt m k = map encryptChar m where
  encryptChar c = lookupCharOrError c k fst

lookupCharOrError :: Char -> GenSubKey -> ((Char, Char) -> Char) -> Char
lookupCharOrError c k f = case find ((c ==) . f) k of 
    Just (c', _) -> c'
    Nothing -> error $ "Cannot find corresponding character for: " ++ show c

changeChar :: (Char, Char) -> GenSubKey -> GenSubKey
changeChar p []     = []
changeChar p (x:xs) = if fst x == fst p then p : xs else x : changeChar p xs 
