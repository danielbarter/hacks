module Partition ( Partition 
                 , toList
                 , fromList
                 , fromList'
                 ) where


import Text.PrettyPrint
import Control.Monad (join)
import Bratelli


data Partition = Empty | NonEmpty Int Partition deriving (Eq, Ord)

toList :: Partition -> [Int]
toList Empty          = []
toList (NonEmpty x p) = x : (toList p)

ppPartition :: Partition -> Doc
ppPartition p =
  foldr ($$) empty [ foldr (<+>) empty (take n $ repeat $ text "*")  | n <- l]
  where l = toList p

instance Show Partition where
  show = render . ppPartition

decreasingAndPositive :: [Int] -> Bool
decreasingAndPositive []        = True
decreasingAndPositive [n]       = (n > 0)
decreasingAndPositive (n:ns)    = (n >= n') && (decreasingAndPositive ns)
  where n' = head ns

fromList' []     = Empty
fromList' (n:ns) = NonEmpty n $ fromList' ns

fromList :: [Int] -> Maybe Partition
fromList l = if (decreasingAndPositive l) then Just $ fromList' l else Nothing
