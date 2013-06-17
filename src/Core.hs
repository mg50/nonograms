{-# LANGUAGE FlexibleInstances #-}

module Core where
import qualified Data.List as L

data Square = Empty | Filled | Unknown deriving (Show, Eq)
newtype Nonogram = Nonogram { rows :: [[Square]] } deriving (Show, Eq)

data Game = Game { solution :: Nonogram,
                   current :: Nonogram,
                   previous :: [Nonogram],
                   next :: [Nonogram]
                 } deriving (Show, Eq)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

class Dimensionable a where
  dimensions :: a -> (Int, Int)

instance Dimensionable [[a]] where
  dimensions xs = (length (head xs), length xs)

instance Dimensionable Nonogram where
  dimensions (Nonogram n) = dimensions n

class Transposable a where
  transpose :: a -> a

instance Transposable [[a]] where
  transpose = L.transpose

instance Transposable Nonogram where
  transpose = Nonogram . transpose . rows

instance Transposable Game where
  transpose (Game soln curr prev next) =
    Game (transpose soln) (transpose curr) (map transpose prev) (map transpose next)

newGame :: Nonogram -> Game
newGame nono = let (x, y) = dimensions nono
                   curr = Nonogram $ replicate y $ replicate x Unknown
               in Game nono curr [] []

squareAt :: Game -> Int -> Int -> Square
squareAt game x y = r !! y !! x
  where r = rows $ current game

columns :: Nonogram -> [[Square]]
columns nono = transpose $ rows nono

partitionRow :: Eq a => [a] -> [[a]]
partitionRow [] = []
partitionRow (x:xs) = reverse $ go [[x]] xs x
  where go acc [] _ = acc
        go acc@(a:as) (x:xs) prev = if x == prev
                                       then go ((x:a):as) xs x
                                       else go ([x]:acc) xs x

hintsForRow :: [Square] -> [Int]
hintsForRow squares = map length $ dropEmpty (partitionRow squares)
  where dropEmpty [] = []
        dropEmpty (x:xs) = if null x || head x /= Filled
                              then dropEmpty xs
                              else x:(dropEmpty xs)

wins :: Nonogram -> Nonogram -> Bool
wins answer guesses = all rowsMatch $ zip (rows answer) (rows guesses)
  where rowsMatch (row1, row2) = all cellsMatch $ zip row1 row2
        cellsMatch (c1, c2) = (c1 == Filled && c2 == c1) ||
                              (c1 == Empty && c2 /= Filled)

gameWon :: Game -> Bool
gameWon game = wins (solution game) (current game)

updateList :: [a] -> a -> Int -> [a]
updateList [] _ _ = []
updateList (x:xs) y idx | idx == 0  = y:xs
                        | otherwise = x:(updateList xs y $ idx - 1)

updateNonogram :: Nonogram -> (Int, Int) -> Square -> Nonogram
updateNonogram (Nonogram rows) (x, y) sq = Nonogram $ updateList rows row y
  where row = updateList (rows !! y) sq x

-- updateGame :: Game -> (Int, Int) -> Square -> Game
-- updateGame game@(Game soln curr prev next) (x, y) sq
--   | newCurr == curr = game
--   | (not . null) next &&
--     newCurr == head next = Game soln newCurr (curr:prev) (tail next)
--   | otherwise = Game soln newCurr (curr:prev) []

--   where newCurr = updateNonogram curr (x, y) sq

updateGame :: Game -> [(Int, Int)] -> Square -> Game
updateGame game@(Game soln curr prev next) coords sq
  | newCurr == curr = game
  | (not . null) next && newCurr == head next =
      Game soln newCurr (curr:prev) (tail next)
  | otherwise = Game soln newCurr (curr:prev) []

  where curr = current game
        newCurr = L.foldl' update curr coords
        update nono (x, y) = updateNonogram nono (x, y) sq

undo :: Game -> Game
undo game@(Game _ _ [] _) = game
undo (Game soln curr (x:xs) next) = Game soln x xs (curr:next)

redo :: Game -> Game
redo game@(Game _ _ _ []) = game
redo (Game soln curr prev (x:xs)) = Game soln x (curr:prev) xs


type Proven = Bool
frontProven :: [[Square]] -> [Int] -> [Proven]
frontProven _ [] = []
frontProven squares hints =
  case squares of
    [] -> notp
    c:[] | head c /= Filled       -> notp
         | length c /= head hints -> notp
         | otherwise              -> [True]

    c:d:cs | head c == Unknown      -> notp
           | head d == Unknown      -> notp
           | head c == Empty        -> frontProven (d:cs) hints
           | length c /= head hints -> notp
           | otherwise              -> True:(frontProven cs $ tail hints)

  where notp = replicate (length hints) False

proven :: [[Square]] -> [Int] -> [Proven]
proven squares hints =
  let front = frontProven squares hints
      back = frontProven (reverse squares) (reverse hints)
      combined = zip front $ reverse back
      chooseProven = uncurry (||)
  in map chooseProven combined


hints :: Nonogram -> Nonogram -> [[(Int, Proven)]]
hints soln nono = let hints = map hintsForRow (rows soln)
                      partitionedRows = map partitionRow (rows nono)
                      provens = map (uncurry proven) $ zip partitionedRows hints
                  in flip map (hints `zip` provens) $ \(h, p) -> zip h p
