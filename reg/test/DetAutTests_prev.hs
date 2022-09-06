module DetAutTests where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.List as List

import Data.Set (fromList)
import Data.List (intersect)

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

q2aTests =
  testGroup "D1: q2a Tests" [

    testCase "q2a number of states test" $
      areEqual
        2
        (length q2aStates),

    testCase "q2a number of letters test" $
      areEqual
        2
        (length q2aAlphabet),

    testCase "q2a start test" $
      areEqual
        'e'
        (start q2a),

    testCase "q2a transitions test" $
      areEqual
        (fromList [('e',S '0','o'),('e',S '1','o'),('o',S '0','e'),('o',S '1','e')])
        (fromList (trans q2a)),
    
    -- The next 2 might be useful if we end up enforcing an ordering to the states and alphabet in the student answers
    -- testCase "q2a transitions test 2" $
    --   areEqual
    --     (fromList [(q2aStates!!0, S '0', q2aStates!!1), (q2aStates!!0, S '1', q2aStates!!1), (q2aStates!!1, S '0', q2aStates!!0), (q2aStates!!1, S '1', q2aStates!!0)])
    --     (fromList (trans q2a)),

    -- testCase "q2a transitions test 3" $
    --   areEqual
    --     (fromList [(q2aStates!!0, S (q2aAlphabet!!0), q2aStates!!1), (q2aStates!!0, S (q2aAlphabet!!1), q2aStates!!1), (q2aStates!!1, S (q2aAlphabet!!0), q2aStates!!0), (q2aStates!!1, S (q2aAlphabet!!1), q2aStates!!0)])
    --     (fromList (trans q2a)),

    testCase "q2a final test" $
      areEqual
        ['e']
        (final q2a),
    
    testCase "q2a reachability test (e, \"\")" $
      areEqual
        (fromList [('e',"")])
        (reachable' (autoTrSys q2a) ('e', "")),

    testCase "q2a reachability test (e, \"0101\")" $
      areEqual
        (fromList [('e',"0101"),('o',"101"),('e',"01"),('o',"1"),('e',"")])
        (reachable' (autoTrSys q2a) ('e', "0101")),
    
    testCase "q2a membership test (\"0\")" $
      areEqual
        False
        (dfaMember q2a "0"),
    
    testCase "q2a membership test (\"1\")" $
      areEqual
        False
        (dfaMember q2a "1"),

    testCase "q2a membership test (\"00\")" $
      areEqual
        True
        (dfaMember q2a "00"),
    
    testCase "q2a membership test (\"11\")" $
      areEqual
        True
        (dfaMember q2a "11"),
    
    testCase "q2a membership test (\"101\")" $
      areEqual
        False
        (dfaMember q2a "101"),
    
    testCase "q2a membership test (\"0101\")" $
      areEqual
        True
        (dfaMember q2a "0101"),
    
    testCase "q2a membership test (\"11111010110\")" $
      areEqual
        False
        (dfaMember q2a "11111010110")
  ]

q2bTests =
  testGroup "D2: q2b Tests" [

    testCase "q2b number of states test" $
      areEqual
        3
        (length q2bStates),

    testCase "q2b number of letters test" $
      areEqual
        2
        (length q2bAlphabet),

    testCase "q2b start test" $
      areEqual
        0
        (start q2b),

    testCase "q2b transitions test" $
      areEqual
        (fromList [(0,S '0',0),(0,S '1',1),(1,S '0',1),(1,S '1',2),(2,S '0',2),(2,S '1',0)])
        (fromList (trans q2b)),

    testCase "q2b final test" $
      areEqual
        [0]
        (final q2b),
    
    testCase "q2b reachability test (0, \"\")" $
      areEqual
        (fromList [(0,"")])
        (reachable' (autoTrSys q2b) (0, "")),

    testCase "q2b reachability test (0, \"0101\")" $
      areEqual
        (fromList [(0,"0101"),(0,"101"),(1,"01"),(1,"1"),(2,"")])
        (reachable' (autoTrSys q2b) (0, "0101"))
  ]

q2cTests =
  testGroup "D3: q2c Tests" [

    testCase "q2c number of states test" $
      areEqual
        6
        (length q2cStates),

    testCase "q2c number of letters test" $
      areEqual
        2
        (length q2cAlphabet),

    testCase "q2c start test" $
      areEqual
        ('e',0)
        (start q2c),

    testCase "q2c transitions test" $
      areEqual
        (fromList [(('e',0),S '0',('o',0)),(('e',0),S '1',('o',1)),(('e',1),S '0',('o',1)),(('e',1),S '1',('o',2)),(('e',2),S '0',('o',2)),(('e',2),S '1',('o',0)),(('o',0),S '0',('e',0)),(('o',0),S '1',('e',1)),(('o',1),S '0',('e',1)),(('o',1),S '1',('e',2)),(('o',2),S '0',('e',2)),(('o',2),S '1',('e',0))])
        (fromList (trans q2c)),

    testCase "q2c final test" $
      areEqual
        [('e',0)]
        (final q2c),
    
    testCase "q2c reachability test (\"e,0\", \"\")" $
      areEqual
        (fromList [(('e',0),"")])
        (reachable' (autoTrSys q2c) (('e',0),"")),

    testCase "q2c reachability test (\"e,0\", \"0101\")" $
      areEqual
        (fromList [(('e',0),"0101"),(('o',0),"101"),(('e',1),"01"),(('o',1),"1"),(('e',2),"")])
        (reachable' (autoTrSys q2c) (('e',0), "0101"))
  ]

q3Tests =
  testGroup "D4: q3 Tests" [

    testCase "q3 number of states test" $
      areEqual
        8
        (length q3States),

    testCase "q3 number of letters test" $
      areEqual
        13
        (length q3Alphabet),

    testCase "q3 start test" $
      areEqual
        '0'
        (start q3),

    testCase "q3 transitions test" $
      areEqual
        (fromList [('0',S '1','1'),('0',S '2','1'),('0',S '3','1'),('0',S '4','1'),('0',S '5','1'),('0',S '6','1'),('0',S '7','1'),('0',S '8','1'),('0',S '9','1'),('0',S '+','j'),('0',S '-','j'),('0',S 'e','j'),('0',S 'E','j'),('1',S '1','1'),('1',S '2','1'),('1',S '3','1'),('1',S '4','1'),('1',S '5','1'),('1',S '6','1'),('1',S '7','1'),('1',S '8','1'),('1',S '9','1'),('1',S '+','j'),('1',S '-','j'),('1',S 'e','4'),('1',S 'E','4'),('2',S '1','3'),('2',S '2','3'),('2',S '3','3'),('2',S '4','3'),('2',S '5','3'),('2',S '6','3'),('2',S '7','3'),('2',S '8','3'),('2',S '9','3'),('2',S '+','j'),('2',S '-','j'),('2',S 'e','j'),('2',S 'E','j'),('3',S '1','3'),('3',S '2','3'),('3',S '3','3'),('3',S '4','3'),('3',S '5','3'),('3',S '6','3'),('3',S '7','3'),('3',S '8','3'),('3',S '9','3'),('3',S '+','j'),('3',S '-','j'),('3',S 'e','4'),('3',S 'E','4'),('4',S '1','6'),('4',S '2','6'),('4',S '3','6'),('4',S '4','6'),('4',S '5','6'),('4',S '6','6'),('4',S '7','6'),('4',S '8','6'),('4',S '9','6'),('4',S '+','5'),('4',S '-','5'),('4',S 'e','j'),('4',S 'E','j'),('5',S '1','6'),('5',S '2','6'),('5',S '3','6'),('5',S '4','6'),('5',S '5','6'),('5',S '6','6'),('5',S '7','6'),('5',S '8','6'),('5',S '9','6'),('5',S '+','j'),('5',S '-','j'),('5',S 'e','j'),('5',S 'E','j'),('6',S '1','6'),('6',S '2','6'),('6',S '3','6'),('6',S '4','6'),('6',S '5','6'),('6',S '6','6'),('6',S '7','6'),('6',S '8','6'),('6',S '9','6'),('6',S '+','j'),('6',S '-','j'),('6',S 'e','j'),('6',S 'E','j'),('j',S '1','j'),('j',S '2','j'),('j',S '3','j'),('j',S '4','j'),('j',S '5','j'),('j',S '6','j'),('j',S '7','j'),('j',S '8','j'),('j',S '9','j'),('j',S '+','j'),('j',S '-','j'),('j',S 'e','j'),('j',S 'E','j')])
        (fromList (trans q3)),

    testCase "q3 final test" $
      areEqual
        (fromList ['3', '6'])
        (fromList (final q3)),
    
    testCase "q3 reachability test (e, \"\")" $
      areEqual
        (fromList [('0',"")])
        (reachable' (autoTrSys q3) ('0', "")),

    testCase "q3 reachability test (0, \"0101\")" $
      areEqual
        (fromList [('0',"0101"),('1',"101"),('1',"01"),('1',"1"),('1',"")])
        (reachable' (autoTrSys q3) ('0', "0101")),
    
    testCase "q3 reachability test (0, \"2.99\")" $
      areEqual
        (fromList [('0',"2.99"),('1',".99"),('2',"99"),('3',"9"),('3',"")])
        (reachable' (autoTrSys q3) ('0', "2.99")),
    
    testCase "q3 reachability test (0, \"23.09e+34\")" $
      areEqual
        (fromList [('0',"23.09e+34"),('1',"3.09e+34"),('1',".09e+34"),('2',"09e+34"),('3',"9e+34"),('4',"e+34"),('5',"+34"),('6',"34"),('6',"4"),('6',"")])
        (reachable' (autoTrSys q3) ('0', "23.09e+34")),
    
    testCase "q3 reachability test (0, \"0.12E−200\")" $
      areEqual
        (fromList [('0',"0.12E−200"),('1',".12E−200"),('2',"12E−200"),('3',"2E−200"),('3',"E−200"),('4',"−200"),('5',"200"),('6',"00"),('6',"0"),('6',"")])
        (reachable' (autoTrSys q3) ('0', "0.12E−200")),
    
    testCase "q3 reachability test (0, \"1.4e1\")" $
      areEqual
        (fromList [('0',"1.4e1"),('1',".4e1"),('2',"4e1"),('3',"e1"),('4',"1"),('6',"")])
        (reachable' (autoTrSys q3) ('0', "1.4e1"))
  ]

q5Tests =
  testGroup "D5: q5 Tests" [

    testCase "q5 number of states test" $
      areEqual
        5
        (length q5States),

    testCase "q5 number of letters test" $
      areEqual
        2
        (length q5Alphabet),

    testCase "q5 start test" $
      areEqual
        '0'
        (start q5),

    testCase "q5 transitions test" $
      areEqual
        (fromList [('0',S '0','0'),('0',S '1','1'),('1',S '0','2'),('1',S '1','4'),('2',S '0','1'),('2',S '1','3'),('3',S '0','3'),('3',S '1','3'),('4',S '0','4'),('4',S '1','3')])
        (fromList (trans q5)),

    testCase "q5 final test" $
      areEqual
        (fromList ['0', '1', '2', '4'])
        (fromList (final q5))
  ]

q6Tests =
  testGroup "D6: q6 Tests" [

    testCase "q6 number of states test" $
      areEqual
        10
        (length q6States),

    testCase "q6 number of letters test" $
      areEqual
        2
        (length q6Alphabet),

    testCase "q6 start test" $
      areEqual
        (0,0)
        (start q6),

    testCase "q6 transitions test" $
      areEqual
        (fromList [((0,0),S 'a',(0,0)),((0,0),S 'b',(0,1)),((0,1),S 'a',(0,0)),((0,1),S 'b',(0,2)),((0,2),S 'a',(0,0)),((0,2),S 'b',(1,2)),((1,0),S 'a',(1,0)),((1,0),S 'b',(1,1)),((1,1),S 'a',(1,0)),((1,1),S 'b',(1,2)),((1,2),S 'a',(1,0)),((1,2),S 'b',(2,2)),((2,0),S 'a',(2,0)),((2,0),S 'b',(2,1)),((2,1),S 'a',(2,0)),((2,1),S 'b',(2,2)),((2,2),S 'a',(2,0)),((2,2),S 'b',(3,0)),((3,0),S 'a',(3,0)),((3,0),S 'b',(3,0))])
        (fromList (trans q6)),

    testCase "q6 final test" $
      areEqual
        (fromList [(3,0)])
        (fromList (final q6))
  ]

q7Tests =
  testGroup "D7: q7 Tests" [

    testCase "q7 number of states test" $
      areEqual
        4
        (length q7States),

    testCase "q7 number of letters test" $
      areEqual
        4
        (length q7Alphabet),

    testCase "q7 start test" $
      areEqual
        '0'
        (start q7),

    testCase "q7 transitions test" $
      areEqual
        (fromList [('0',S (0,0),'0'),('0',S (0,1),'j'),('0',S (1,0),'j'),('0',S (1,1),'1'),('1',S (0,0),'j'),('1',S (0,1),'0'),('1',S (1,0),'2'),('1',S (1,1),'j'),('2',S (0,0),'1'),('2',S (0,1),'j'),('2',S (1,0),'j'),('2',S (1,1),'2'),('j',S (0,0),'j'),('j',S (0,1),'j'),('j',S (1,0),'j'),('j',S (1,1),'j')])
        (fromList (trans q7)),

    testCase "q7 final test" $
      areEqual
        (fromList ['0'])
        (fromList (final q7))
  ]