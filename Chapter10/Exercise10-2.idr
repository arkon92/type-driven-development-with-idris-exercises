import Data.List.Views
import Data.Vect.Views
import Data.Nat.Views
import Data.Vect

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc rec) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc rec) | Empty = []
    equalSuffix (zs ++ [x]) (xs ++ [y]) | (Snoc zsrec) | (Snoc xsrec) = case x == y of
                                                                             False => []
                                                                             True => equalSuffix zs xs | zsrec | xsrec ++ [x]

-- *Exercise10-2> equalSuffix [1, 2, 4, 5] [1..5]
-- [4, 5] : List Integer

-- *Exercise10-2> equalSuffix [1, 2, 3, 4, 5, 6] [1..5]
-- [] : List Integer

-- *Exercise10-2> equalSuffix [1, 2, 4, 5, 6] [1..6]
-- [4, 5, 6] : List Integer


mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)

-- *Exercise10-2> mergeSort [5, 1, 4, 3, 2, 6, 8, 7, 9]
-- [1, 2, 3, 4, 5, 6, 7, 8, 9] : Vect 9 Integer


toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = toBinary n | rec ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n | rec ++ "1"

-- *Exercise10-2> toBinary 42
-- "101010" : String

-- *Exercise10-2> toBinary 94
-- "1011110" : String


palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = case x == y of
                                                     False => False
                                                     True => palindrome ys | rec

-- *Exercise10-2> palindrome (unpack "abccba")
-- True : Bool

-- *Exercise10-2> palindrome (unpack "abcba")
-- True : Bool

-- *Exercise10-2> palindrome (unpack "abcb")
-- False : Bool