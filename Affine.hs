module Affine (
    getKeys,
    encrypt,
    decrypt
) where

import Util
import Data.Maybe (mapMaybe)

type EncodedChar = Int

n = 26

getKeys :: String -> [(Int, Int)]
getKeys cipher = do
  let freqs = dictToList $ frequencyAnalysis cipher
      fullFreqs :: [(Char, Char)]
      fullFreqs = zip (map fst englishFreq) (map fst freqs)
      encodedFreqs :: [(Int, Int)]
      encodedFreqs = mapTuple encodeChar fullFreqs
      getABPair :: (Int, Int) -> Maybe (Int, Int)
      getABPair (x2, y2) = do
        let (x1, y1) = head encodedFreqs
        x2x1 <- inv (x2-x1) n
        let a = ((y2-y1) * x2x1) `emod` n
        let b = (y1 - a * x1) `emod` n
        pure (a, b)
      in mapMaybe getABPair (tail encodedFreqs)

encrypt :: String -> (Int, Int) -> String
encrypt m (a, b) = 
  let encryptChar x = decodeChar $ (a * encodeChar x + b) `emod` n in
      map encryptChar m

decrypt :: String -> (Int, Int) -> Maybe String
decrypt m (a, b) = 
  let decryptChar :: Char -> Maybe Char
      decryptChar y = do
        aInv <- inv a n
        let encoded = (aInv * (encodeChar y - b)) `emod` n
        pure $ decodeChar encoded
      in mapM decryptChar m
