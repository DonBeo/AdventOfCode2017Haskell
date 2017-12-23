
import System.IO

filename :: FilePath
filename = "/Users/lucapuggini/Documents/AdventOfCode/data/data_ch2_p2.txt"


tableToMatrix filename = do
 x <- readFile filename
 let rows = lines x
 let digits_x = map words rows
 let numbers = map (map read) digits_x :: [[Int]]
 return numbers


main = do
 x <- tableToMatrix filename
 print $ x