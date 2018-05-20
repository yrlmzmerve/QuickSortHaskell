import Data.List
qsort [] = []
qsort (x1:x2) = qsort minVal ++ pivot ++ qsort maxVal
    where
        pivot = [y | y<-x2, y==x1] ++ [x1]
        minVal = [y | y<-x2, y<x1]
        maxVal = [y | y<-x2, y>x1]
main = do
    let qS = [ 14, 8, 22, 1, 9, 53, 4, 16, 5]
    let newQS = qsort qS
    putStr "Quick Sort => ";
    print newQS
    