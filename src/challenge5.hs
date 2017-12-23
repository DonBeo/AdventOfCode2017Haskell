
import Data.Array
import System.IO

listToArray l =
  let n_elem = length l
      pos_val = zip (range (0, n_elem)) l
  in array (0, n_elem-1) pos_val
 
getData filename = do
  s <- readFile filename
  let l = map read (lines s) ::[Int]
      a = listToArray l 
  return a

-- Part 1

updatePosArray i a =
  let i_val = a ! i
  in (i+i_val, a//[(i, i_val + 1)])

solution1 a n_steps i
 | i >= length a || i < 0 = n_steps
 | otherwise =
    let ai = updatePosArray i a
    in solution1 (snd ai) (n_steps+1) (fst ai)

-- Part 2

updatePosArray2 i a =
  let i_val = a ! i
  in
    if i_val>=3 then (i+i_val, a//[(i, i_val-1)])
    else (i+i_val, a//[(i, i_val+1)])
      
solution2 a n_steps i
 | i >= length a || i < 0 = n_steps
 | otherwise =
    let ai = updatePosArray2 i a
    in solution2 (snd ai) (n_steps+1) (fst ai)


main = do
  x <- getData "/Users/lucapuggini/Documents/AdventOfCode/data/data_ch5_p1.txt"
  let x_ex = array (0,4) [(0, 0), (1, 3), (2, 0), (3, 1), (4, -3)]

  let n_steps_ex1 = solution1 x_ex 0 0
  print $ n_steps_ex1

  let n_steps1 = solution1 x 0 0
  print $ n_steps1

  let n_steps_ex2 = solution2 x_ex 0 0
  print $ n_steps_ex2

  -- very slow. Probably due to the immutable array
  let n_steps2 = solution2 x 0 0
  print $ n_steps2
  
