import qualified Data.Map as Map
import Control.Monad
import Data.Ord (comparing, Down (Down))
import Data.List (sortBy, sortOn)
import Data.Char (digitToInt, ord, toUpper, chr)
import Data.Maybe (mapMaybe)
import qualified Affine
import qualified GenSub
import Util

main = do
  cipher1 <- parseCipher "cipher1.txt"
  --cipher2 <- parseCipher "cipher2.txt"
  let affineDict = frequencyAnalysis cipher1
  --let gensubDict = frequencyAnalysis cipher2
  let affineKeys = Affine.getKeys cipher1
  --let gensubKeys1 = GenSub.getKeys cipher2
  --let gensubKeys2 = GenSub.switchKey ('F', 'W') gensubKeys1
  let affineDecrypts = mapMaybe (Affine.decrypt cipher1) affineKeys
  --let gensubDecrypt = GenSub.decrypt cipher2 gensubKeys1
  --printDict cipherDict2
  print affineDecrypts
  print $ Affine.decrypt cipher1 (19, 4)

