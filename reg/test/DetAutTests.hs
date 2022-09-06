module DetAutTests where

import Test.Tasty
import Test.Tasty.HUnit

import TrSys
import Automata
import DetAut

areEqual :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
areEqual = assertEqual ""

detAutTests =
  testGroup "Deterministic Automata Tests" [q1Tests, q2aTests, q2bTests, q2cTests, q3Tests, q5Tests, q6Tests, q7Tests]

q1Tests = 
  testGroup "D0: Q1 Tests" [

    testCase "Test q1a" $
      areEqual
        [("q0", "12212"), ("q1", "2212"), ("q1", "212"), ("q1", "12"), ("q2", "2"), ("q3", "e")]
        q1a,

    testCase "Test q1b" $
      areEqual
        [("q0", "12212"), ("q0", "2212"), ("q0", "212"), ("q0", "12"), ("q0", "2"), ("q0", "e")]
        q1b,

    testCase "Test q1c" $
      areEqual
        [("q0", "12212"), ("q1", "2212"), ("q2", "212"), ("q4", "12"), ("q4", "2"), ("q4", "e")]
        q1c
  ]

mkGoodCases cat m =
  map (\s -> testCase (cat ++ " Correct " ++ (show s)) $ assertBool "" $ dfaMember m s)
mkBadCases cat m =
  map (\s -> testCase (cat ++ " Incorrect " ++ (show s)) $ assertBool "" $ not $ dfaMember m s)

q2aTests =
  testGroup "D1: q2a Membership Tests" $
    mkGoodCases "q2a" q2a good
    ++ mkBadCases "q2a" q2a bad
  where
    good    = ["", "00", "11", "1111", "0110", "010101", "01010101"]
    bad     = ["0", "1", "000", "010", "111", "01010", "000110011"]

q2bTests =
  testGroup "D2: q2b Membership Tests" $
    mkGoodCases "q2b" q2b good
    ++ mkBadCases "q2b" q2b bad
  where
    good    = ["", "00", "111", "10101", "10110", "111111", "101010111"]
    bad     = ["1", "01", "001", "011", "101", "11111", "000110011"]

q2cTests =
  testGroup "D3: q2c Membership Tests" $
    mkGoodCases "q2c" q2c good
    ++ mkBadCases "q2c" q2c bad
  where
    good    = ["", "00", "1110", "1011", "101010", "010101", "1010101110"]
    bad     = ["1", "01", "001", "010", "111", "01110", "00011001110"]

q3Tests =
  testGroup "D4: q3 Membership Tests" $
    mkGoodCases "q3" q3 good
    ++ mkBadCases "q3" q3 bad
  where
    good    = ["2.99", "23.09e+34", "0.12E-200", "1.4e1"]
    bad     = ["", "42", ".01", "1.1.1", "-1", "+5", "0.1E0e", "e101", "01e01.0", "1.00E+1.1"]

q5Tests =
  testGroup "D5: q5 Membership Tests" $
    mkGoodCases "q5" q5 good
    ++ mkBadCases "q5" q5 bad
  where
    good    = ["", "00", "11", "1001", "10010", "01001", "10000100"]
    bad     = ["101", "0101", "101", "10101", "01010", "010010010", "10000100100001"]

q6Tests =
  testGroup "D6: q6 Membership Tests" $
    mkGoodCases "q6" q6 good
    ++ mkBadCases "q6" q6 bad
  where
    good    = ["bbbbb", "abbbbbaa", "bbbabbbabbb", "bbbbabbb"]
    bad     = ["", "a", "b", "bbb", "abbbabbba", "aaaaa", "abbabbabbabba"]

q7Tests =
  testGroup "D7: q7 Membership Tests" $
    mkGoodCases "q7" q7 good
    ++ mkBadCases "q7" q7 bad
  where
    good    = [[(0,0)], [(0,0),(0,0)], [(1,1),(0,1)], [(1,1),(1,0),(1,1),(0,0),(0,1)]]
    bad     = [[(0,1)],[(1,1)], [(1,1),(1,0),(0,1)], [(1,1),(0,1),(0,1)], [(1,1),(1,0),(1,1),(0,0),(0,1),(1,0)]]