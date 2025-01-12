import BB.Set
import Data.List (nub, sort, sortOn)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude hiding (maximum, minimum, null)

propertyTests :: TestTree
propertyTests =
  testGroup
    "property tests"
    [ testProperty "height of the tree is logarithmic" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
              h = fromIntegral (height s) :: Double
              lim = logBase (fromIntegral (omega + 1) / fromIntegral omega) (fromIntegral (size s + 1)) :: Double
           in h <= lim,
      testProperty "size of the set is number of unique elems in the list" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in size s === length (nub xs),
      testProperty "the set contains all elems of the list" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in all (`member` s) xs,
      testProperty "the first item of the set is the smallest" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in at 0 s === minimum s,
      testProperty "the last item of the set is the greatest" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in at (length (nub xs) - 1) s === maximum s,
      testProperty "splitLt returns a set with all elems less than the pivot" $
        \x xs ->
          let s = foldr insert empty (xs :: [Int])
           in all (< x) $ splitLt x s,
      testProperty "splitGt returns a set with all elems greater than the pivot" $
        \x xs ->
          let s = foldr insert empty (xs :: [Int])
           in all (> x) $ splitGt x s,
      testProperty "union is commutative" $
        \xs ys ->
          let s = foldr insert empty (xs :: [Int])
              t = foldr insert empty (ys :: [Int])
           in null $ difference (s `union` t) (t `union` s),
      testProperty "intersection is commutative" $
        \xs ys ->
          let s = foldr insert empty (xs :: [Int])
              t = foldr insert empty (ys :: [Int])
           in null $ difference (s `intersection` t) (t `intersection` s),
      testProperty "union is associative" $
        \xs ys zs ->
          let s = foldr insert empty (xs :: [Int])
              t = foldr insert empty (ys :: [Int])
              u = foldr insert empty (zs :: [Int])
           in null $ difference (s `union` t `union` u) (t `union` u `union` s),
      testProperty "intersection is associative" $
        \xs ys zs ->
          let s = foldr insert empty (xs :: [Int])
              t = foldr insert empty (ys :: [Int])
              u = foldr insert empty (zs :: [Int])
           in null $ difference (s `intersection` t `intersection` u) (t `intersection` u `intersection` s),
      testProperty "union is distributive over intersection" $
        \xs ys zs ->
          let s = foldr insert empty (xs :: [Int])
              t = foldr insert empty (ys :: [Int])
              u = foldr insert empty (zs :: [Int])
           in null $ difference (t `intersection` u `union` s) ((s `union` t) `intersection` (s `union` u)),
      testProperty "intersection is distributive over union" $
        \xs ys zs ->
          let s = foldr insert empty (xs :: [Int])
              t = foldr insert empty (ys :: [Int])
              u = foldr insert empty (zs :: [Int])
           in null $ difference (t `union` u `intersection` s) ((s `intersection` t) `union` (s `intersection` u)),
      testProperty "union is idempotent" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in null $ difference (s `union` s) s,
      testProperty "intersection is idempotent" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in null $ difference (s `intersection` s) s,
      testProperty "union of a set and an empty set is the first set" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in null $ difference (s `union` empty) s,
      testProperty "intersection of a set and an empty set is an empty set" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in null $ intersection s empty,
      testProperty "toAscList returns a list sorted in ascending order" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in toAscList s === sort (nub xs),
      testProperty "toDescList returns a list sorted in descending order" $
        \xs ->
          let s = foldr insert empty (xs :: [Int])
           in toDescList s === sortOn negate (nub xs)
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [ testCase "an empty set is empty" $ True @=? null empty,
      testCase "a singeleton set is not empty" $ False @=? null (singleton ()),
      testCase "size of an empty set is 0" $ 0 @=? size empty,
      testCase "size of a singleton set is 1" $ 1 @=? size (singleton ()),
      testCase "height of an empty set is 0" $ 0 @=? height empty,
      testCase "height of a singleton set is 1" $ 1 @=? height (singleton ()),
      testCase "the first item of an empty set is absent" $ Nothing @=? at 0 (empty :: Set ()),
      testCase "the first item of a singleton set is present" $ Just () @=? at 0 (singleton ()),
      testCase "the second item of a singleton set is absent" $ Nothing @=? at 1 (singleton ())
    ]

tests :: TestTree
tests = testGroup "set tests" [propertyTests, unitTests]

main :: IO ()
main = defaultMain tests
