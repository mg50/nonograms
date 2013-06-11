{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI.Curses where
import Control.Monad.Trans
import Control.Monad.Trans.State
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import UI
import Core
import Control.Monad

data Cursor = Cursor { point :: (Int, Int)
                     , mark :: Maybe (Int, Int) }

newtype CursesUI a = CursesUI { runCursesUI :: StateT Cursor IO a  }


data CellUIState = CellUIState Guess Bool


charFor :: Guess -> Char
charFor Nothing = ' '
charFor (Just Filled) = '#'
charFor (Just Empty) = '.'

cellWidth = 3
cellHeight = 2

coordList :: Int -> Int -> [(Int, Int)]
coordList x y = [(i, j) | i <- [0..(x - 1)], j <- [0..(y - 1)]]

between :: Int -> (Int, Int) -> Bool
between a (x, y) = a `elem` [x..y]

data CellType = Point | Marked | Plain

drawGrid game x y = do
  let nono@(Nonogram squares n) = nonogram game
  (pX, pY) <- liftM point get
  mark <- liftM mark get
  let (dimX, dimY) = dimensions nono
  liftIO $ forM_ (coordList dimX dimY) $ \(cellX, cellY) -> do
    let guess = guessAt game cellX cellY
    let cellType = case mark of
                     _ | cellX == pX && cellY == pY -> Point
                     Just (mkX, mkY) | cellX `between` (mkX, pX) &&
                                       cellY `between` (mkY, pY) -> Marked
                     _ -> Plain
    drawCell cellX cellY guess cellType


castEnum = toEnum . fromEnum

drawCell :: Int -> Int -> Guess -> CellType -> IO ()
drawCell cellX cellY guess cellType = do
  let c = charFor guess
  let (baseX, baseY) = (cellX*cellWidth, cellY*cellHeight)
  let coords = [(baseX + dx, baseY + dy) | dx <- [0..(cellWidth - 1)],
                                           dy <- [0..(cellHeight - 1)]]
  forM_ coords $ \(x, y) -> mvAddCh x y (castEnum c)
  refresh


blah x = do
  initCurses
  keypad stdScr True
  echo False
  cursSet CursorInvisible
  erase
  drawCell x 0 (Just Filled) Point
  blah (x + 1)






-- selectedCellStyle = Style WhiteF DarkBlueB
-- unselectedCellStyle = Style WhiteF BlackB
-- provenClueStyle = AttributeStyle [Dim] WhiteF BlackB
-- unprovenClueStyle = Style WhiteF BlackB

-- charFor :: Guess -> Char
-- charFor Nothing = ' '
-- charFor (Just Filled) = '#'
-- charFor (Just Empty) = '.'

-- cellWidth = 3
-- cellHeight = 2

-- castEnum = toEnum . fromEnum

-- drawCell :: Guess -> Int -> Int -> CursesUI ()
-- drawCell guess x y = CursesUI $ do
--   let c = charFor guess
--   let coords = [(x+dx, y+dy) | dx <- [0..(cellWidth-1)], dy <- [0..(cellHeight-1)]]
--   liftIO $ forM_ coords $ \(x, y) -> do
--     mvAddCh x y (castEnum c)




-- -- drawCell cellState = CursesUI $ do


-- initializeUI = do
--   initCurses
--   keypad stdScr True
--   echo False
--   cursSet CursorInvisible
--   (sizeY, sizeX) <- scrSize

--   endWin

-- instance UI CursesUI where
--   display game = undefined
--   promptGuesses game = undefined
