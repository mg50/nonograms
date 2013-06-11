module Core where
import Data.List
import qualified Data.Map as M
import Data.Maybe

data Square = Empty | Filled deriving (Show, Eq)
data Nonogram = Nonogram [Square] Int deriving (Show, Eq)

type Guess = Maybe Square
type Guesses = M.Map Int Square

data Game = Game { nonogram :: Nonogram, history :: [Guesses] } deriving (Show, Eq)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

newGame :: Nonogram -> Game
newGame nono = Game nono [M.fromList []]

dimensions :: Nonogram -> (Int, Int)
dimensions (Nonogram squares n) = (n, length squares `div` n)

guessAt :: Game -> Int -> Int -> Guess
guessAt (Game nono@(Nonogram _ n) hist) x y = M.lookup idx cur
  where cur = head hist
        (x, y) = dimensions nono
        idx = x*n + y

rows :: [a] -> Int -> [[a]]
rows xs n = go xs [] n
  where go _ acc 0 = acc
        go remaining acc counter =
          go (drop n remaining) ((take n remaining):acc) $! counter - 1

nonogramRows :: Nonogram -> [[Square]]
nonogramRows (Nonogram [] _) = []
nonogramRows (Nonogram squares n) = rows squares n

nonogramCols :: Nonogram -> [[Square]]
nonogramCols = Data.List.transpose . nonogramRows

part :: Eq a => [a] -> [[a]]
part [] = []
part (x:xs) = reverse $ go [[x]] xs x
  where go acc [] _ = acc
        go acc@(a:as) (x:xs) prev = if x == prev
                                       then go ((x:a):as) xs x
                                       else go ([x]:acc) xs x

clues :: [Square] -> [Int]
clues squares = map length $ dropEmpty (part squares)
  where dropEmpty [] = []
        dropEmpty (x:xs) = if head x == Empty
                              then dropEmpty xs
                              else x:(dropEmpty xs)


wins :: Nonogram -> Guesses -> Bool
wins (Nonogram squares n) guesses = not $ any isWrong squaresWithGuesses
  where squaresWithGuesses = map pair $ zip squares [0..]
        pair (sq, idx) = (sq, M.lookup idx guesses)
        isWrong (square, guess) = case square of
                                    Filled -> guess /= Just Filled
                                    Empty  -> guess == Just Filled

updateGuesses :: Guesses -> [(Int, Guess)] -> Guesses
updateGuesses guesses [] = guesses
updateGuesses guesses ((idx, guess):rest) = updateGuesses (updated guesses idx guess) rest
  where updated mapping idx x = case x of
                                  Nothing -> M.delete idx mapping
                                  Just sq -> M.insert idx sq mapping


type Proven = Bool
frontProven :: [[Guess]] -> [Int] -> [Proven]
frontProven guesses ints =
  case guesses of
    [] -> notp
    c:[] | head c /= Just Filled -> notp
         | length c /= head ints -> notp
         | otherwise             -> [True]

    c:d:cs | head c == Nothing     -> notp
           | head d == Nothing     -> notp
           | head c == Just Empty  -> frontProven (d:cs) ints
           | length c /= head ints -> notp
           | otherwise             -> True:(frontProven cs $ tail ints)

  where notp = replicate (length ints) False

proven :: [[Guess]] -> [Int] -> [Proven]
proven guesses ints =
  let front = frontProven guesses ints
      back = frontProven (reverse guesses) (reverse ints)
      combined = zip front $ reverse back
      chooseProven = uncurry (||)
  in map chooseProven combined
