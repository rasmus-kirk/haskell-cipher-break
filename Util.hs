module Util (
    englishFreq,
    encodeChar,
    decodeChar,
    emod,
    inv,
    dictToList,
    mapTuple,
    frequencyAnalysis,
    printDict,
    parseCipher,
    CipherDictionary,
    EncodedChar
) where

import Data.Char (ord, toUpper, chr)
import qualified Data.Map as Map
import Data.List (find, sortOn)
import Data.Ord (Down(..))
import Control.Monad (forM_)

type EncodedChar = Int
type CipherDictionary = Map.Map Char Int

-- From wikipedia
englishFreq :: [(Char, Float)]
englishFreq = [
    ('E', 12.702),
    ('T', 09.056),
    ('A', 08.167),
    ('O', 07.507),
    ('I', 06.966),
    ('N', 06.749),
    ('S', 06.327),
    ('H', 06.094),
    ('R', 05.987),
    ('D', 04.253),
    ('L', 04.025),
    ('C', 02.782),
    ('U', 02.758),
    ('M', 02.406),
    ('W', 02.360),
    ('F', 02.228),
    ('G', 02.015),
    ('Y', 01.974),
    ('P', 01.929),
    ('B', 01.492),
    ('V', 00.978),
    ('K', 00.772),
    ('J', 00.153),
    ('X', 00.150),
    ('Q', 00.095),
    ('Z', 00.074)
  ]

frequencyAnalysis :: String -> CipherDictionary
frequencyAnalysis =
  foldl f (Map.fromAscList [])
  where
  f :: CipherDictionary -> Char -> CipherDictionary
  f dict cipherChar = case Map.lookup cipherChar dict of
    Just i -> Map.insert cipherChar (i+1) dict
    Nothing -> Map.insert cipherChar 1 dict

encodeChar :: Char -> Int
encodeChar c = ord (toUpper c) - ord 'A'

decodeChar :: Int -> Char
decodeChar i = toUpper (chr (i + ord 'A'))

emod :: Int -> Int -> Int
emod a b = let m = a `mod` b in
  if m < 0 then
    emod m b
  else m

extendedEu :: Int -> Int -> (Int, Int, Int)
extendedEu a 0 = (a, 1, 0)
extendedEu a b = (gcd, x, y)
    where 
      (gcd, x1, y1) = extendedEu b (a `mod` b)
      x = y1
      y = x1 - (a `div` b) * y1

inv :: Int -> Int -> Maybe Int
inv a n = let (gcd, x, _) = extendedEu a n in
    case gcd of
      1 -> Just (x `emod` n) 
      _ -> Nothing

dictToList :: CipherDictionary -> [(Char, Int)]
dictToList dict = sortOn (Down . snd) $ Map.toList dict

mapTuple :: (a -> b) -> [(a, a)] -> [(b, b)]
mapTuple f = map (\(x, y) -> (f x, f y))

printDict :: CipherDictionary -> IO()
printDict dict = forM_ (dictToList dict) (\x -> putStrLn $ show (fst x) ++ " : " ++ show (snd x))

parseCipher :: FilePath -> IO String
parseCipher path = fmap (filter (/= '\n')) (readFile path)