module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Fcd

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [distanceTests]

distanceTests :: TestTree
distanceTests = testGroup "Longest common subsequence tests"
  [ testCase "Some LCS" $ do
      0 @=? lengthSub (lcs "" "42")
      0 @=? lengthSub (lcs "abc" "def")
      1 @=? lengthSub (lcs "ab" "b")
      1 @=? lengthSub (lcs "ab" "ba")
      4 @=? lengthSub (lcs "hello" "hullo")
      5 @=? lengthSub (lcs "my word" "their words")
      4 @=? lengthSub (lcs "abcdef" "acedf")
  , testCase "Sort candidates according to the distance to a reference" $
      ["abcd", "ab", "efg"] @=? sortCandidates ["ab", "abcd", "efg"] "abdc"
  ]


