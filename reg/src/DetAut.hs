module DetAut where
  
import Data.Char (isDigit)
import qualified Data.Set as Set

import TrSys
import Automata

-- Definitions: -----------------------------------------------------------------------

-- -- | /TrSys a/ is the type of transition systems over configurations of
-- -- type /a/.  A value of this type is just a function in /a -> Set a/
-- -- which yields the set of successors of any configuration (those configurations
-- -- that are reached from the given one in exactly one step). 
-- type TrSys a = a -> Set a

-- -- | Given an ordered type /a/, a transition system /tr/ over /a/ and
-- -- a configuration /c/ of the system, /reachable' tr c/ is the set of
-- -- all configurations that are reachable from /c/ according to /tr/.
-- reachable' :: Ord a => TrSys a -> a -> Set a

-- | /Auto a q/ is the type of automata, represented as a record
-- containing three fields:
--   - /start m/ is the initial state of automaton /m/.
--   - /final m/ is a list of the final states of automaton /m/.
--   - /trans m/ is the list of all transitions of automaton /m/,
--     with each transition represented by a tuple of the form /(q1, l, q2)/
-- data Auto a q =
--   MkAuto {
--     states   :: Set q,
--     alphabet :: Set a,
--     start    :: q,
--     trans    :: Set (q, Label a, q),
--     final    :: Set q
--   } deriving (Show)

-- -- | Given an automaton /m/, /autoTrSys m/ is the associated transition system,
-- -- whose configurations are of the form (q, w), where:
-- --   - /q/ is a state of /m/
-- --   - /w/ is a word over the alphabet of /m/
-- autoTrSys :: (Ord a, Ord q) => Auto a q -> TrSys (q, [a])

-- -- | Given a __deterministic__ automaton /m/ and a word /w/,
-- -- /dfaMember m w/ just if /w/ is accepted by /m/.  
-- dfaMember :: (Ord a, Ord q) => Auto a q -> [a] -> Bool


-- a transition function takes in a state and a letter and produces the next state
-- qXtrans :: q -> a -> q
 
-- takes the transition function, a list of states and the alphabet, and returns a list of transitions
mkTrans :: (Ord q, Ord a) => (q -> a -> q) -> [q] -> [a] -> [(q, Label a, q)]
mkTrans f [] as = []
mkTrans f qs [] = []
mkTrans f (q:qs) (a:as) = [(q, S a, f q a)]++(mkTrans f [q] as)++(mkTrans f qs (a:as))


-- Answers: ---------------------------------------------------------------------------

-- q1 - Second part? 
q1a = [("q0", "12212"), ("q1", "2212"), ("q1", "212"), ("q1", "12"), ("q2", "2"), ("q3", "e")]
q1b = [("q0", "12212"), ("q0", "2212"), ("q0", "212"), ("q0", "12"), ("q0", "2"), ("q0", "e")]
q1c = [("q0", "12212"), ("q1", "2212"), ("q2", "212"), ("q4", "12"), ("q4", "2"), ("q4", "e")]

-- q2a
q2aTrans :: Char -> Char -> Char
q2aTrans 'e' _ = 'o'
q2aTrans 'o' _ = 'e'

q2a :: Auto Char Char
q2a =
  MkAuto {
    states = Set.fromList ['e', 'o'],
    alphabet = Set.fromList ['0', '1'],
    start = 'e',
    trans = Set.fromList (mkTrans q2aTrans ['e', 'o'] ['0', '1']),
    final = Set.fromList ['e']
  }

-- q2b
q2bTrans :: Int -> Char -> Int
q2bTrans q '0' = q
q2bTrans x '1' = (x + 1) `mod` 3

q2b :: Auto Char Int
q2b =
  MkAuto {
    states = Set.fromList [0, 1, 2],
    alphabet = Set.fromList ['0', '1'],
    start = 0,
    trans = Set.fromList (mkTrans q2bTrans [0, 1, 2] ['0', '1']),
    final = Set.fromList [0]
  }

-- q2c
q2cStates = [('e',0), ('e',1), ('e',2), ('o',0), ('o',1), ('o',2)]

q2cTrans :: (Char, Int) -> Char -> (Char, Int)
q2cTrans ('e',a) '0' = ('o',a)
q2cTrans ('o',a) '0' = ('e',a)
q2cTrans ('e',x) '1' = ('o', (x+1) `mod` 3)
q2cTrans ('o',x) '1' = ('e', (x+1) `mod` 3)

q2c :: Auto Char (Char, Int)
q2c =
  MkAuto {
    states = Set.fromList q2cStates,
    alphabet = Set.fromList ['0', '1'],
    start = ('e',0),
    trans = Set.fromList (mkTrans q2cTrans q2cStates ['0', '1']),
    final = Set.fromList [('e',0)]
  }

-- q3
q3States = ['0', '1', '2', '3', '4', '5', '6', 'j']
q3Alphabet = ['0'..'9'] ++ ['.', '+', '-', 'e', 'E']

q3Trans :: Char -> Char -> Char
q3Trans q a
  | q `elem` "01" && isDigit(a)    = '1'
  | q `elem` "23" && isDigit(a)    = '3'
  | q `elem` "456" && isDigit(a)   = '6'
  | q == '1' && a == '.'           = '2'
  | q `elem` "13" && a `elem` "eE" = '4'
  | q == '4' && a `elem` "+-"      = '5'
  | otherwise                      = 'j'

q3 :: Auto Char Char
q3 =
  MkAuto {
    states = Set.fromList q3States,
    alphabet = Set.fromList q3Alphabet,
    start = '0',
    trans = Set.fromList (mkTrans q3Trans q3States q3Alphabet),
    final = Set.fromList (['3', '6'])
  }

-- q4
q4 :: Ord q => Auto a q -> Auto a q
q4 a = 
  MkAuto {
    states = (states a),
    alphabet = (alphabet a),
    start = (start a),
    trans = (trans a),
    final = Set.difference (states a) (final a)
  }

-- q5
q5States = ['0', '1', '2', '3', '4']

q5Trans :: Char -> Char -> Char
q5Trans '0' '0' = '0'
q5Trans '0' '1' = '1'
q5Trans '1' '0' = '2'
q5Trans '1' '1' = '4'
q5Trans '2' '0' = '1'
q5Trans '2' '1' = '3'
q5Trans '3'  _  = '3'
q5Trans '4' '0' = '4'
q5Trans '4' '1' = '3'

q5 :: Auto Char Char
q5 =
  MkAuto {
    states = Set.fromList q5States,
    alphabet = Set.fromList ['0', '1'],
    start = '0',
    trans = Set.fromList (mkTrans q5Trans q5States ['0', '1']),
    final = Set.fromList ['0', '1', '2', '4']
  }

-- q6
q6States = [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2), (3,0)]

q6Trans :: (Int,Int) -> Char -> (Int,Int)
q6Trans (3,0)  _  = (3,0)
q6Trans (x,_) 'a' = (x,0)
q6Trans (x,0) 'b' = (x,1)
q6Trans (x,1) 'b' = (x,2)
q6Trans (0,2) 'b' = (1,2)
q6Trans (1,2) 'b' = (2,2)
q6Trans (2,2) 'b' = (3,0)

q6 :: Auto Char (Int,Int)
q6 =
  MkAuto {
    states = Set.fromList q6States,
    alphabet = Set.fromList ['a', 'b'],
    start = (0,0),
    trans = Set.fromList (mkTrans q6Trans q6States ['a', 'b']),
    final = Set.fromList [(3,0)]
  }

-- q7
q7States = ['0', '1', '2', 'j']
q7Alphabet = [(0,0), (0,1), (1,0), (1,1)]

q7Trans :: Char -> (Int,Int) -> Char
q7Trans '0' (0,0) = '0'
q7Trans '0' (1,1) = '1'
q7Trans '1' (0,1) = '0'
q7Trans '1' (1,0) = '2'
q7Trans '2' (0,0) = '1'
q7Trans '2' (1,1) = '2'
q7Trans  _    _   = 'j'

q7 :: Auto (Int,Int) Char
q7 =
  MkAuto {
    states = Set.fromList q7States,
    alphabet = Set.fromList q7Alphabet,
    start = '0',
    trans = Set.fromList (mkTrans q7Trans q7States q7Alphabet),
    final = Set.fromList ['0']
  }