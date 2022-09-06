module RegLanTests where

import Test.Tasty
import Test.Tasty.HUnit

import TrSys
import Automata
import RegLan

regLanTests =
  testGroup "Regular Languages Tests" [q1Tests, q2aTests, q2bTests, q2cTests, q3Tests, q4Tests]

mkGoodCases cat m =
  map (\s -> testCase (cat ++ " Correct " ++ (show s)) $ assertBool "" $ member m s)
mkBadCases cat m =
  map (\s -> testCase (cat ++ " Incorrect " ++ (show s)) $ assertBool "" $ not $ member m s)

q1Tests =
  testGroup "RL0: q1 Membership Tests" $
    mkGoodCases "q2a" q1 good
    ++ mkBadCases "q2a" q1 bad
  where
    good    = ["00", "0000", "001110", "01010110"]
    bad     = ["0", "1", "01", "100", "011", "101", "11111", "000110011"]

q2aTests =
  testGroup "RL1: q2a Membership Tests" $
    mkGoodCases "q2a" q2a good
    ++ mkBadCases "q2a" q2a bad
  where
    good    = ["ab", "aab", "abb", "acbab", "acbab", "bbab", "cbbbab"]
    bad     = ["", "a", "b", "c", "bca", "baca", "adcbc", "bacacba"]
 
q2bTests =
  testGroup "RL2: q2b Membership Tests" $
    mkGoodCases "q2b" q2b good
    ++ mkBadCases "q2b" q2b bad
  where
    good    = ["b", "bac", "cb", "abcdb", "bacb", "bacdbac"]
    bad     = ["", "a", "c", "ac", "acd", "bcad", "abcd", "abcdcbca"]

q2cTests =
  testGroup "RL3: q2c Membership Tests" $
    mkGoodCases "q2c" q2c good
    ++ mkBadCases "q2c" q2c bad
  where
    good    = [[1], [2], [0,1], [0,2], [1,2], [1,0], [2,0], [1,2,0], [2,0,1], [1,0,2], [2,1,2]]
    bad     = [[0], [0,0], [1,1], [2,2], [0,1,1], [0,2,2], [0,1,1,0], [0,2,2,0], [1,0,2,1,2], [2,1,2,0,1], [0,1,2,0,2,0,1], [0,2,0,1,2,1,2]]

q3Tests =
  testGroup "RL4: q3 Membership Tests" $
    mkGoodCases "q3" (revAutomata q2a) good
    ++ mkBadCases "q3" (revAutomata q2a) bad
  where
    good    = ["aba", "aaba", "abba", "acbab", "acbab", "bbab", "cbbbab"]
    bad     = ["", "a", "b", "c", "bca", "bca", "adcbc", "acacb"]

mkGoodCases' cat m =
  map (\s -> testCase (cat ++ " Correct " ++ (show s)) $ assertBool "" $ member (m s) s)
mkBadCases' cat m =
  map (\s -> testCase (cat ++ " Incorrect " ++ (show s)) $ assertBool "" $ not $ member (m s) s)

q4Tests =
  testGroup "RL5: q4 Membership Tests" $
    mkGoodCases' "q4" (tailAutomata q2a) good
    ++ mkBadCases' "q4" (tailAutomata q2a) bad
  where
    good    = ["abab", "aaba", "ababa", "acbab", "acbab", "bbab", "cbbbab"]
    bad     = ["a", "b", "c", "bca", "baca", "adcbc", "bacacba"]