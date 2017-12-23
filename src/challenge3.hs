#!/usr/bin/env runhaskell

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Maybe 
import Data.List



moves = ["r", "u", "l", "d"]

infSequence s = cycle moves

isOrigin x y = x == 0 && y == 0
isURCorner x y = x == y && x > 0 
isULCorner x y = x == (-y) && y > 0
isLLCorner x y = x == y && y < 0
isLRCornerP x y = x == (-y+1)  && y <=0

nextPos x y "r" = (x+1, y)
nextPos x y "u" = (x, y+1)
nextPos x y "l" = (x-1, y)
nextPos x y "d" = (x, y-1)

nextMove x y cur_move
 | isOrigin x y = "r"
 | isURCorner x y = "l"
 | isULCorner x y = "d"
 | isLLCorner x y = "r"
 | isLRCornerP x y = "u"
 | otherwise = cur_move

getListOfMoves n moves x y pos
 | n == 1 = (pos, moves)
 | otherwise = 
     let n_move = nextMove x y (head moves)
         next_pos = nextPos x y n_move
         new_x = fst next_pos
         new_y = snd next_pos
     in getListOfMoves (n-1) (n_move:moves) new_x new_y (next_pos:pos)


-- Part 2

getNeigs x y = [(x-1, y-1),
                (x, y-1),
                (x+1, y-1),
                (x-1, y),
                (x, y),
                (x+1, y),
                (x-1, y+1),
                (x, y+1),
                (x+1, y+1)]

getSumNeigs x y pos2val =
 let
   neigs = getNeigs x y
   map_list = Prelude.map (\x -> Map.lookup x pos2val) neigs
  in sum (Data.Maybe.catMaybes map_list)

genGrid target move x y pos2val last_val
 | last_val > target = last_val
 | otherwise =
    let sum_neigs = getSumNeigs x y pos2val
        n_move = nextMove x y move
        next_pos = nextPos x y n_move
        new_x = fst next_pos
        new_y = snd next_pos
        new_pos2val = Map.insert (x,y) sum_neigs pos2val
    in genGrid target n_move new_x new_y new_pos2val sum_neigs

  
main = do
 -- Part 1
 let res = getListOfMoves 312051 [] 0 0 []
 let pos =  head (fst res)
 print $ (abs (fst pos)) + (abs (snd pos))

 -- Part 2
 let res = genGrid 312051 "r" 0 0 (Map.fromList [((0,0), 1)]) 1
 print $ res
