
import qualified Data.Set as Set
import System.IO
import qualified Data.List as List

-- read the data
filename :: FilePath
filename = "/Users/lucapuggini/Documents/AdventOfCode/data/data_ch4_p1.txt"

getData filename = do
 x <- readFile filename
 return (map words (lines x))

-- Part 1

hasNotDuplicate l =
 length l == length s
 where s = Set.fromList l 

solution1 l = length (filter hasNotDuplicate l) 

-- Part 2

sortWordsInList l = map List.sort l

solution2 l =
 let sorted_list = map sortWordsInList l
 in length (filter hasNotDuplicate sorted_list)

    
main = do
 x <- getData filename
 print $ (solution1 x)
 print $ (solution2 x)
