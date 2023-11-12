module Main

import Data.Vect

invert : Bool -> Bool
invert False = True
invert True = False

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail = " ++ show xs

total allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = not (isEven k)

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

fourInts : Vect 4 Int
fourInts = [0,1,2,3]

sixInts : Vect 6 Int
sixInts = [4,5,6,7,8,9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

total vectLengths : Vect len String -> Vect len Nat
vectLengths [] = []
vectLengths (x :: xs) = length x :: vectLengths xs

insert : Ord e => (x : e) -> (xsSorted : Vect len e) -> Vect (S len) e
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord e => Vect n e -> Vect n e
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

total my_length : Vect len e -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : Vect len e -> Vect len e
my_reverse [] = ?my_reverse_rhs_0
my_reverse (x :: xs) = ?my_reverse_rhs_1

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

total my_vect_map : (a -> b) -> Vect len a -> Vect len b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

vectSum : Num ntype => Vect r ntype -> Vect r ntype -> Vect r ntype
vectSum [] [] = []
vectSum (x :: xs) (y :: ys) = let r = x + y in
                                  r :: vectSum xs ys

-- Exercise 2. implement add matrix
addMatrix : Num ntype => 
            Vect rows (Vect cols ntype) -> 
            Vect rows (Vect cols ntype) -> 
            Vect rows (Vect cols ntype)
addMatrix [] [] = []
addMatrix (x::xs) (y::ys) = let n = vectSum x y in
                                n :: addMatrix xs ys

-- Exercise 3. implment mult matrix
mulMatrix : Num ntype => 
            Vect n (Vect m ntype) -> 
            Vect m (Vect p ntype) -> 
            Vect n (Vect p ntype)

createEmpties : {n : _} -> Vect n (Vect 0 e)
createEmpties = replicate n []

-- Exercise 1. re-implement with zipWith
transposeMat : {n : _} -> Vect m (Vect n t) -> Vect n (Vect m t)
transposeMat [] = createEmpties 
transposeMat (x :: xs) = let xsTrans = transposeMat xs in 
                             zipWith (::) x xsTrans 
