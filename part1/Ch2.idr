module Main

{-export
average : (str : String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str)) in
                  cast totalLength / cast numWords

  where
    wordCount : String -> Nat
    wordCount str = List.length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map List.length strs

showAverage : String -> String
showAverage str = "The average word length is: " ++ show (average str) ++ "\n"
-}

double : Num ty => ty -> ty
double x = x + x

add : Int -> Int -> Int
add x y = x + y

twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type
rotate : Shape -> Shape

quadruple : Num a => a -> a
quadruple = twice double

turn_around : Shape -> Shape
turn_around = twice rotate

longer : String -> String -> Nat
longer word1 word2
  = let len1 = length word1
        len2 = length word2 in
        if len1 > len2 then len1 else len2

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x 

main : IO ()
main = putStrLn "hello" -- ?repl "Enter a string: " showAverage

-- Exercises
-- 1.
-- a) ("A", "B", "C") : (String, (String, String))
-- b) ["A", "B", "C"] : List String
-- c) (('A', "B"), "C") : ((Char, String), String)

-- 2. 
palindrome : String -> Bool
palindrome str = str == (reverse str)

-- 3.
palindrome' : List Char -> Bool
palindrome' str = let str1 = mkLower str in
                      str1 == (reverse str1)
  where
    mkLower : List Char -> List Char
    mkLower [] = []
    mkLower (x::xs) = toLower x :: mkLower xs

-- 4. & 5.
palindrome'' : Nat -> String -> Bool
palindrome'' s str = if length str < s then False else palindrome str

-- 6.
counts : List Char -> (Nat, Nat)
counts str = (countWords str, length str)
  where
    countWords : List Char -> Nat 
    countWords [] = 0
    countWords (x::xs) = if x == ' ' then 1 + countWords xs else countWords xs
