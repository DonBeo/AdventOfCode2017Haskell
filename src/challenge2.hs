#!/usr/bin/env runhaskell



import System.IO

-- read the data
filename2 :: FilePath
filename2 = "/Users/lucapuggini/Documents/AdventOfCode/data/data_ch2_p2.txt"


tableToMatrix filename = do
 x <- readFile filename
 let rows = lines x
 let digits_x = map words rows
 let numbers = map (map read) digits_x :: [[Int]]
 return numbers



-- Part 1
listRange a b [] = b - a
listRange a b (h:t) = listRange (min a h) (max b h) t

getListRange (h:t) = listRange h h (h:t)
 
solution1 x = sum (map getListRange x)


-- Part 2     
allpairs xs = [ (x1,x2) | x1 <- xs, x2 <- xs, x1 /= x2 ]

areDivisors a b = mod a b == 0

evenlyDivisors (h:t) = 
  let (a, b) = h
  in if areDivisors a b then div a b
  else evenlyDivisors t
  
getEvenlyDivision l = 
  let p = allpairs l
  in evenlyDivisors p 

solution2 x = sum (map getEvenlyDivision x)


main = do
  let x1 = [[5,9,2,8], [9, 4, 7, 3], [3, 8, 6, 5]]
  print $ solution1 x1

  x2 <- tableToMatrix filename2
  print $ solution2 x2
