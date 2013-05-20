import System.Random
import Data.List 

calculateTriangle n = foldr h (g,[])  [1..n]
                  where g = mkStdGen n
                        h   n (g,acc) = let (ng,l) = generateRow n g in (ng,l:acc)

generateRow n g = foldl h (g,[]) [1..n]
                 where h (g,l) _ = let (n,ng) = next g in (ng,n:l)

renderTriangle :: [[Int]] -> String
renderTriangle = unlines .  (map (unwords . (intersperse " ") .  (map show)))

main = do 
          let triangle_string = renderTriangle . snd $ calculateTriangle 100
          writeFile "triangle.txt" triangle_string