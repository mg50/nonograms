module Solver where
import qualified Data.Map as M
import qualified Data.List as L
import Core (Nonogram, hintsForNonogram)

data ZDD = Bottom
         | Top
         | Node { value :: Int, hi :: ZDD, lo :: ZDD } deriving (Eq)

instance Show ZDD where
  show = go 0
    where go n Top = indents n ++ "T"
          go n Bottom = indents n ++ "B"
          go n (Node val hi lo) = indents n ++ (show val) ++ ":\n" ++
                                  go (n+2) hi ++ "\n" ++ go (n+2) lo
          indents n = replicate n ' '

union :: ZDD -> ZDD -> ZDD
union Bottom z = z
union z Bottom = z
union Top Top = Top
union Top z = case z of
  Node x hi Bottom -> Node x hi Top
  _ -> let lo' = union Top (lo z) in z{lo = lo'}
union z Top = union Top z
union z1 z2 | z1 == z2 = z1
            | value z1 == value z2 =
                let hi' = union (hi z1) (hi z2)
                    lo' = union (lo z1) (lo z2)
                in Node (value z1) hi' lo'
            | value z1 < value z2 =
                let lo' = union (lo z1) z2
                in Node (value z1) (hi z1) lo'
            | otherwise = union z2 z1

join Bottom _ = Bottom
join _ Bottom = Bottom
join Top x = x
join x Top = x
join n1@(Node v1 hi1 lo1) n2@(Node v2 hi2 lo2)
  | v1 == v2 = Node v1 (join hi1 hi2 `union` join hi1 lo2 `union` join lo1 hi2) (join lo1 lo2)
  | v1 < v2 = Node v1 (join hi1 n2) (join lo1 n2)
  | otherwise = join n2 n1

intersection :: ZDD -> ZDD -> ZDD
intersection z Bottom = Bottom
intersection Bottom z = Bottom
intersection Top z = case z of
  Top -> Top
  _   -> intersection Top (lo z)
intersection z Top = intersection Top z
intersection z1 z2 | z1 == z2 = z1
                   | value z1 == value z2 =
                       let hi' = intersection (hi z1) (hi z2)
                           lo' = intersection (lo z1) (lo z2)
                       in Node (value z1) hi' lo'
                   | value z1 < value z2 = intersection (lo z1) z2
                   | value z1 > value z2 = intersection z2 z1

toList :: ZDD -> [[Int]]
toList zdd = L.nub $ go zdd
  where go Top = [[]]
        go Bottom = []
        go (Node val hi lo) = go lo ++ map (val:) (go hi)

tails [] = []
tails (x:xs) = (x:xs):(tails xs)

contiguous :: Int -> [Int] -> ZDD
contiguous n row = go n row
  where go n remaining = case remaining of
          _ | n > length remaining -> Bottom
            | n == 0 -> Top
          [] -> Top
          (r:rs) -> let hi = go (n-1) rs
                        lo = Bottom
                    in Node r hi lo

rowZdd :: [Int] -> [Int] -> ZDD
rowZdd hints [] = Top
rowZdd [] row = Top
rowZdd hints row = foldr1 union $ map (go hints) (tails row)
  where go hints row
          | score hints > length row = Bottom
          | otherwise = let h:hs = hints
                            cont = contiguous h row
                        in cont `join` rowZdd hs (drop (h + 1) row)

        score [] = 0
        score xs = sum xs + length xs - 1

makeRow start numCols =
  take numCols [start..]

makeColumn start numRows numCols =
  take numRows [start,(start+numCols)..]

for = flip map

zdd :: [[Int]] -> [[Int]] -> ZDD
zdd rowHints colHints =
  let y = length rowHints
      x = length colHints
      row = [0..x-1]
      col = [0..y-1]
      rowZdds = for (rowHints `zip` col) $ \(rowHint, rowNum) ->
        let row' = makeRow (x*rowNum) y in rowZdd rowHint row'
      colZdds = for (colHints `zip` row) $ \(colHint, colNum) ->
        let col' = makeColumn colNum x y in rowZdd colHint col'
      merge = foldr1 join
  in (merge rowZdds) `intersection` (merge colZdds)

hasUniqueSolution :: Nonogram -> Bool
hasUniqueSolution nono =
  let hints = hintsForNonogram nono
      nonoZdd = uncurry zdd hints
  in case toList nonoZdd of
    _:[] -> True
    _    -> False
