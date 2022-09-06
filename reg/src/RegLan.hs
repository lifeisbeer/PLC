module RegLan where

import Data.List (elemIndices)

import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

import TrSys
import Automata
import DetAut (mkTrans)

-- Definitions: -----------------------------------------------------------------------

-- -- takes the transition function, a list of states and the alphabet, and returns a list of transitions
-- mkTrans :: (Ord q, Ord a) => (q -> a -> q) -> [q] -> [a] -> [(q, Label a, q)]


-- takes the transition function, a list of states and the alphabet, and returns a list of transitions
mkTransNFA :: (Ord q, Ord a) => (q -> a -> [q]) -> [q] -> [a] -> [(q, Label a, q)]
mkTransNFA f [] as = []
mkTransNFA f qs [] = []
mkTransNFA f (q:qs) (a:as) 
  | null res  = (mkTransNFA f [q] as)++(mkTransNFA f qs (a:as))
  | otherwise = [(q, S a, q') | q' <- res]++(mkTransNFA f [q] as)++(mkTransNFA f qs (a:as))
  where res = f q a


-- Answers: ---------------------------------------------------------------------------

-- q1
q1States = ["{0}", "{1}", "{1,2}", "{}"]

q1Trans :: [Char] -> Char -> [Char]
q1Trans "{0}"   '0' = "{1}"
q1Trans "{0}"   '1' = "{}"
q1Trans "{1}"   '0' = "{1,2}"
q1Trans "{1}"   '1' = "{1}"
q1Trans "{1,2}" '0' = "{1,2}"
q1Trans "{1,2}" '1' = "{1}"
q1Trans "{}"     _  = "{}"

q1 :: Auto Char [Char]
q1 =
  MkAuto {
    states = Set.fromList q1States,
    alphabet = Set.fromList ['0', '1'],
    start = "{0}",
    trans = Set.fromList (mkTrans q1Trans q1States ['0', '1']),
    final = Set.fromList ["{1,2}"]
  }

-- q2a
q2aStates = ['0', '1', '2']
q2aAlphabet = ['a', 'b', 'c']

q2aTrans :: Char -> Char -> [Char]
q2aTrans '0' 'a' = ['0', '1']
q2aTrans '0'  _  = ['0']
q2aTrans '1' 'b' = ['2']
q2aTrans '2'  _  = ['2']
q2aTrans  _   _  = []

q2a :: Auto Char Char
q2a =
  MkAuto {
    states = Set.fromList q2aStates,
    alphabet = Set.fromList q2aAlphabet,
    start = '0',
    trans = Set.fromList (mkTransNFA q2aTrans q2aStates q2aAlphabet),
    final = Set.fromList ['2']
  }

-- q2b
q2bStates = ['0', '1', '2']
q2bAlphabet = ['a', 'b', 'c', 'd']

q2bTrans :: Char -> Char -> [Char]
q2bTrans '0' 'b' = ['0', '1']
q2bTrans '0'  _  = ['0']
q2bTrans '1' 'a' = ['2']
q2bTrans '1' 'b' = ['2']
q2bTrans '1' 'd' = ['2']
q2bTrans '2'  _  = ['2']
q2bTrans  _   _  = []

q2b :: Auto Char Char
q2b =
  MkAuto {
    states = Set.fromList q2bStates,
    alphabet = Set.fromList q2bAlphabet,
    start = '0',
    trans = Set.fromList (mkTransNFA q2bTrans q2bStates q2bAlphabet),
    final = Set.fromList ['1','2']
  }

-- q2c
q2cStates = [0, 1, 2, 3]
q2cAlphabet = [0, 1, 2]

q2cTrans :: Int -> Int -> Int
q2cTrans i j = (i*3+j) `mod` 4

q2c :: Auto Int Int
q2c =
  MkAuto {
    states = Set.fromList q2cStates,
    alphabet = Set.fromList q2cAlphabet,
    start = 0,
    trans = Set.fromList (mkTrans q2cTrans q2cStates q2cAlphabet),
    final = Set.fromList [1, 2, 3]
  }


-- q3
data MaybeNew a = State a | New
  deriving (Eq, Ord)

instance Show a => Show (MaybeNew a) where
  show New = "New"
  show (State a) = show a

-- revTrans takes in a list of transitions, a list of final states, 
-- and the alphabet and produces the list of reverse transitions
-- plus all transitions from New to the every final state
revTrans :: [(q, Label a, q)] -> [q] -> [a] -> [((MaybeNew q), Label a, (MaybeNew q))]
revTrans [] fs as = [(New, S a, State q) | q <- fs, a <- as]
revTrans ((q, S a, p):ts) fs as = (State p, S a, State q) : (revTrans ts fs as)


-- revAutomata takes in an automaton, and return the reverse automaton (accepts the reversed words)
revAutomata :: (Ord q, Ord a) => Auto a q -> Auto a (MaybeNew q)
revAutomata a =
  MkAuto {
    states = Set.fromList (New:(map State (Set.toList (states a)))),
    alphabet = alphabet a,
    start = New,
    trans = Set.fromList (revTrans (Set.toList (trans a)) (Set.toList (final a)) (Set.toList (alphabet a))),
    final = Set.fromList [State (start a)]
  }


-- q4
addE :: Ord q => (Eq q, Eq a) => [(q, Label a, q)] -> q -> a -> [((MaybeNew q), Label a, (MaybeNew q))]
addE ts q f = [(New, E, State p) | (q0, a, p) <- ts, a == S f, q0 == q] ++ (prepare ts)
  where
    prepare :: [(q, Label a, q)] -> [((MaybeNew q), Label a, (MaybeNew q))]
    prepare [] = []
    prepare ((q0, S a, p):ts) = (State q0, S a, State p):(prepare ts)

tailAutomata :: (Eq q, Ord q, Eq a, Ord a) => Auto a q -> [a] -> Auto a (MaybeNew q)
tailAutomata a input =
  MkAuto {
    states = Set.fromList (New:(map State (Set.toList (states a)))),
    alphabet = alphabet a,
    start = New,
    trans = Set.fromList (addE (Set.toList (trans a)) (start a) (head input)),
    final = Set.fromList [State q | q <- (Set.toList (final a))]
  }
  

-- functions for pumping lemma (not used atm):

-- findPos finds the position of repeated elements in the input
-- elemIndices :: Eq a => a -> [a] -> [Int]
-- isUnique :: Eq a => a -> [a] -> Maybe Bool
findPos :: Eq a => [a] -> (Int, Int)
findPos input = findRep input input
  where
    findRep :: Eq a => [a] -> [a] -> (Int, Int)
    findRep (x:xs) input
      | (x `notElem` xs) == False = ( pos !! 0, pos !! 1 )
      | otherwise = (findRep xs input)
      where pos = (elemIndices x input)

-- findUVW finds the u, v and w in the input
findUVW :: Eq a => [a] -> ([a], [a], [a])
findUVW input = (u, v, w)
  where
    (p1, p2) = findPos input
    u = take p1 input
    v = take (p2-p1) (drop p1 input)
    w = drop (p2) input