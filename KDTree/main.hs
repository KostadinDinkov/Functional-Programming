--
data TreeNode point = Fork point (TreeNode point) (TreeNode point) | Empty deriving(Show)

class Point t where
    dimension :: t -> Int
    coordinate :: Int -> t -> Double
    distance :: t -> t -> Double

data P2d = P2d Double Double deriving(Eq, Show)

instance Point P2d where
    dimension _ = 2
    coordinate 0 (P2d x y) = x
    coordinate 1 (P2d x y) = y
    distance (P2d x1 y1) (P2d x2 y2) = sqrt $ ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)

medianAtLevel :: Point a => Int -> [a] -> Double
medianAtLevel level points = sortedTargetCoordinates!!(length points `div` 2)
 where
    sortedTargetCoordinates = mergeSort targetCoordinates
    targetCoordinates = map (coordinate level) points

sortForLevel :: Point t => Int -> [t] -> [t]
sortForLevel _ [] = []
sortForLevel _ [x] = [x]
sortForLevel level xs = sortForLevel level firstHalf `mergeForLevel` sortForLevel level secondHalf
  where mid = length xs `quot` 2
        (firstHalf, secondHalf) = splitAt mid xs
        mergeForLevel [] xs = xs
        mergeForLevel xs [] = xs
        mergeForLevel (x:xs) (y:ys)
          {-| coordinate level y < coordinate level x = y : mergeForLevel (x:xs) ys-}
          | compareAtLevel level y x = y : mergeForLevel (x:xs) ys
          | otherwise = x : mergeForLevel xs (y:ys)

equal :: Point p => p -> p -> Bool
equal left right = and[c left == c right| d <- [0..(dimension left - 1)], c <- [coordinate d]]

compareAtLevel :: Point p => Int -> p -> p -> Bool
compareAtLevel level left right
 |coordinate level left == coordinate level right = compareAtLevel ((level + 1) `mod` (dimension left)) left right
 |otherwise = coordinate level left < coordinate level right

insertPointInNodeAtLevel :: Point t => Int -> t -> TreeNode t -> TreeNode t
insertPointInNodeAtLevel level point Empty = Fork point Empty Empty
insertPointInNodeAtLevel level point (Fork root left right)
 |equal point root = Fork root left right
 |compareAtLevel level point root = Fork root (insertPointInNodeAtLevel ((level + 1) `mod` (dimension root)) point left) right
 |otherwise = Fork root left (insertPointInNodeAtLevel ((level + 1) `mod` (dimension root)) point right)



treeFromListAtLevel :: Point p => Int -> [p] -> TreeNode p
treeFromListAtLevel level [] = Empty
treeFromListAtLevel level points = Fork forkPoint leftTree rightTree
 where sortedPoints = sortForLevel level points
       forkPoint = sortedPoints!!(length points `div` 2)
       rest = let (ys,zs) = splitAt (length points `div` 2) sortedPoints in ys ++ (tail zs)
       leftTree = treeFromListAtLevel ((level + 1) `mod` (dimension forkPoint)) $ filter (\point -> coordinate level point < coordinate level forkPoint) rest
       rightTree = treeFromListAtLevel ((level + 1) `mod` (dimension forkPoint)) $ filter (\point -> not $ coordinate level point < coordinate level forkPoint) rest

nearestNeighbour :: Point p => p -> TreeNode p -> p
nearestNeighbour origin (Fork root Empty Empty) = root
nearestNeighbour origin (Fork root left Empty) = if (distance origin root) < (distance origin (nearestNeighbour origin left)) then root else (nearestNeighbour origin left)
nearestNeighbour origin (Fork root Empty right) = if (distance origin root) < (distance origin (nearestNeighbour origin right)) then root else (nearestNeighbour origin right)
nearestNeighbour origin (Fork root left right)
 |(distance origin root) < (distance origin (nearestNeighbour origin left)) = if (distance origin root) < (distance origin (nearestNeighbour origin right)) then root else (nearestNeighbour origin right)
 |(distance origin root) < (distance origin (nearestNeighbour origin right)) = if (distance origin root) < (distance origin (nearestNeighbour origin left)) then root else (nearestNeighbour origin left)
 |(distance origin (nearestNeighbour origin left)) < (distance origin (nearestNeighbour origin right)) = (nearestNeighbour origin left)
 |otherwise = (nearestNeighbour origin right)

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort firstHalf `merge` mergeSort secondHalf
  where mid = length xs `quot` 2
        (firstHalf, secondHalf) = splitAt mid xs
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | y < x = y : merge (x:xs) ys
          | otherwise = x : merge xs (y:ys)