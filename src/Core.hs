module Core where
import Data.List
import qualified Data.Map as M
import Data.Maybe

data Square = Empty | Filled deriving (Show, Eq)
data Nonogram = Nonogram [Square] Int deriving (Show, Eq)

type Clue = Maybe Square
type Clues = M.Map Int Square

data Game = Game { nonogram :: Nonogram, history :: [Clues] } deriving (Show, Eq)

newGame :: Nonogram -> Game
newGame nono = Game nono [M.fromList []]

rows :: Nonogram -> [[Square]]
rows (Nonogram [] _) = []
rows (Nonogram squares n) = go squares [] n
  where go _ acc 0 = acc
        go remaining acc counter =
          go (drop n remaining) ((take n remaining):acc) $! counter - 1

cols :: Nonogram -> [[Square]]
cols = Data.List.transpose . rows

part :: Eq a => [a] -> [[a]]
part [] = []
part (x:xs) = reverse $ go [[x]] xs x
  where go acc [] _ = acc
        go acc@(a:as) (x:xs) prev = if x == prev
                                       then go ((x:a):as) xs x
                                       else go ([x]:acc) xs x

count :: [Square] -> [Int]
count squares = map length $ dropEmpty (part squares)
  where dropEmpty [] = []
        dropEmpty (x:xs) = if head x == Empty
                              then dropEmpty xs
                              else x:(dropEmpty xs)


wins :: Nonogram -> Clues -> Bool
wins (Nonogram squares n) clues = any isWrong squaresWithClues
  where squaresWithClues = map (\(sq, idx) -> (sq, M.lookup idx clues)) $ zip squares [0..]
        isWrong (square, clue) = case square of
                                   Filled -> clue /= Just Filled
                                   Empty  -> clue == Just Filled

updateClues :: Clues -> [(Int, Clue)] -> Clues
updateClues clues [] = clues
updateClues clues ((idx, clue):rest) = updateClues (updated clues idx clue) rest
  where updated mapping idx x = case x of
                                  Nothing -> M.delete idx mapping
                                  Just _  -> M.insert x idx mapping


data EpistemicStatus = Proven | NotProven deriving (Show, Eq)
frontProven clues ints =
  case clues of
    [] -> notp
    c:[] | head c /= Just Filled -> notp
         | length c /= head ints -> notp
         | otherwise             -> [Proven]

    c:d:cs | head c == Nothing     -> notp
           | head d == Nothing     -> notp
           | head c == Just Empty  -> frontProven (d:cs) ints
           | length c /= head ints -> notp
           | otherwise             -> Proven:(frontProven cs $ tail ints)

  where notp = replicate (length ints) NotProven

proven clues ints =
  let front = frontProven clues ints
      back = frontProven (reverse clues) (reverse ints)
      combined = zip front $ reverse back
      chooseProven = \(x, y) -> if x == Proven || y == Proven then Proven else NotProven
  in map chooseProven combined
