
import qualified Data.Maybe 

getList l = Data.Maybe.catMaybes l


main = do
  let l = [Just 3, Nothing, Just 5]
  print $ getList l
