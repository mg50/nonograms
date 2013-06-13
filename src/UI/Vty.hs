{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module UI.Vty where
import Graphics.Vty.Widgets.All
import Graphics.Vty hiding (Cursor)
import Graphics.Vty.Attributes
import Core
import qualified Data.List as L
import qualified Data.Text as T
import UI
import System.Exit
import Action
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

data CellUIType = Plain | Point | Marked deriving (Show)
data CellUI = CellUI { square :: Square, cellUIType :: CellUIType }

data VtyData = VtyData { cells :: [[Widget FormattedText]]
                       , rowHints :: [[Widget FormattedText]]
                       , colHints :: [[Widget FormattedText]]
                       , point :: (Int, Int)
                       , mark :: Maybe (Int, Int)
                       , keyChan :: TChan (Key, [Modifier])
                       , done :: MVar ()}
type VtyM a = StateT VtyData IO a

newtype VtyIO a = VtyIO { unVtyIO :: IO a }


data Direction = Up | Down | Lft | Rgt
coordsForDirection dir = case dir of
  Up   -> (0, -1)
  Down -> (0, 1)
  Lft  -> (-1, 0)
  Rgt  -> (1, 0)

movePoint :: Game -> Direction -> VtyM ()
movePoint game dir = let (dimX, dimY) = dimensions (current game)
                         (dx, dy) = coordsForDirection dir
                     in modify $ \cursor -> do
                       let (x, y) = point cursor
                       let x' = x + dx
                       let y' = y + dy
                       if x' `elem` [0..dimX-1] && y' `elem` [0..dimY-1]
                          then cursor{point=(x', y')}
                          else cursor

clearMark :: VtyM ()
clearMark = modify $ \cursor -> cursor{mark=Nothing}

markIsSet :: VtyM Bool
markIsSet = do m <- gets mark
               case m of
                 Nothing -> return False
                 _       -> return True

setMarkAtPoint :: VtyM ()
setMarkAtPoint = modify $ \cursor -> let pt = (point cursor)
                                     in cursor{mark=(Just pt)}

pointAlignedWithMark :: Direction -> VtyM Bool
pointAlignedWithMark dir = do (px, py) <- gets point
                              let (dx, dy) = coordsForDirection dir
                              let px' = px + dx
                              let py' = py + dy
                              mk <- gets mark
                              return $ case mk of
                                         Nothing -> False
                                         Just (mx, my) | px' == mx  -> True
                                                       | py' == my  -> True
                                                       | otherwise  -> False

setPointAtMark :: VtyM ()
setPointAtMark = do mk <- gets mark
                    case mk of
                      Nothing -> return ()
                      Just m -> modify $ \info -> info{point = m}

setCells :: [[Widget FormattedText]] -> VtyM ()
setCells cells = modify $ \vty -> vty{cells=cells}

squareAtMark :: Game -> VtyM Square
squareAtMark game = do (x, y) <- gets point
                       return $ squareAt game x y

cellWidth = 4
cellHeight = 2

charFor Filled = '#'
charFor Empty = '.'
charFor Unknown = ' '

type Point = (Int, Int)
between :: Point -> (Point, Point) -> Bool
between (x, y) ((a1, b1), (a2, b2)) | x == a1 && x == a2 && elem y ([b1..b2] ++ [b2..b1]) = True
                                    | y == b1 && y == b2 && elem x ([a1..a2] ++ [a2..a1]) = True
                                    | otherwise                                           = False


to :: Int -> Int -> [Int]
to x y = if null [x..y] then [y..x] else [x..y]

selectedCoords :: VtyM [(Int, Int)]
selectedCoords = do (px, py) <- gets point
                    mk <- gets mark
                    case mk of
                      Nothing -> return [(px, py)]
                      Just (mx, my) | px == mx -> return $ map (\n -> (px, n)) $ py `to` my
                                    | py == my -> return $ map (\n -> (n, py)) $ px `to` mx

getUIType :: (Int, Int) -> VtyM CellUIType
getUIType coords = do
  pt <- gets point
  mk <- gets mark
  return $ case mk of
             _ | pt == coords -> Point
             Just m | coords `between` (m, pt) -> Marked
             _ -> Plain

squareText :: Square -> T.Text
squareText square = charFor square |> replicate cellWidth
                                   |> replicate cellHeight
                                   |> L.intercalate "\n"
                                   |> T.pack

cellStyle Plain  = Attr Default Default Default
cellStyle Point  = Attr Default (SetTo black) (SetTo white)
cellStyle Marked = Attr Default (SetTo black) (SetTo bright_blue)

createCells :: Int -> Int -> IO [[Widget FormattedText]]
createCells x y = replicateM y $ replicateM x $ plainText ""

scheduleM :: VtyM () -> VtyM ()
scheduleM action = StateT $ \s -> do schedule $ (runStateT action s >> return ())
                                     return ((), s)

formatGrid :: Game -> VtyM ()
formatGrid game = do
  c <- gets cells
  let nono = current game
  forM_ (c `zip` [0..]) $ \(row, rowNum) -> do
    forM_ (row `zip` [0..]) $ \(widget, colNum) -> do
      uiType <- getUIType (colNum, rowNum)
      let style = cellStyle uiType
          txt = squareText $ squareAt game colNum rowNum
      liftIO $ setTextWithAttrs widget [(txt, style)]


emptyVtyData :: IO VtyData
emptyVtyData = do keyCh <- newTChanIO
                  done <- newEmptyMVar
                  return $ VtyData [] [] [] (0, 0) Nothing keyCh done

padList :: a -> [[a]] -> [[a]]
padList _ [] = []
padList x xss = let l = maximum (map length xss)
                in flip map xss $ \xs -> let n = length xs
                                         in replicate (l-n) x ++ xs

paddedHints :: Nonogram -> Nonogram -> [[Maybe (Int, Proven)]]
paddedHints soln nono = nono |> hints soln
                             |> map (map Just)
                             |> padList Nothing

createRowHints :: Nonogram -> IO [[Widget FormattedText]]
createRowHints soln = do let h = paddedHints soln soln
                             (x, y) = (length $ head h, length h)
                         replicateM y $ replicateM x $ plainText ""


createColHints :: Nonogram -> IO [[Widget FormattedText]]
createColHints nono = do x <- createRowHints (transpose nono)
                         return $ transpose x

styleForProven :: Proven -> Attr
styleForProven True = Attr Default Default Default
styleForProven False = Attr (SetTo bold) Default Default

formatRowHints :: Game -> [[Widget FormattedText]] -> IO ()
formatRowHints game hints = do
  let soln = solution game
      curr = current game
      pHints = paddedHints soln curr
  forM_ (zip hints [0..]) $ \(row, rowNum) -> do
    forM_ (zip row [0..]) $ \(col, colNum) -> do
      case pHints !! rowNum !! colNum of
        Nothing -> return ()
        Just (num, proven) -> let widget = hints !! rowNum !! colNum
                                  vpadding = replicate (cellHeight - 1) '\n'
                                  txt = T.pack $ "\n  " ++ show num ++ vpadding ++ " "
                                  style = styleForProven proven
                              in setTextWithAttrs widget [(txt, style)]

formatColHints :: Game -> [[Widget FormattedText]] -> IO ()
formatColHints game hints = formatRowHints (transpose game) (transpose hints)


toTable :: (RowLike a) => [[a]] -> [ColumnSpec] -> BorderStyle -> IO (Widget Table)
toTable xss cols border = do
  rows <- forM xss $ \xs -> xs |> map mkRow
                               |> foldl1 (.|.)
                               |> return
  tbl <- newTable cols border
  sequence_ $ map (addRow tbl) rows
  return tbl

redraw game = scheduleM $ do formatGrid game
                             rh <- gets rowHints
                             ch <- gets colHints
                             liftIO $ formatRowHints game rh
                             liftIO $ formatColHints game ch

initializeM :: Game -> VtyM ()
initializeM game = do
    let nono = solution game
        (x, y) = dimensions nono
    rows <- liftIO $ createCells x y
    rh <- liftIO $ createRowHints (solution game)
    ch <- liftIO $ createColHints (solution game)
    modify $ \vty -> vty{cells = rows,
                         rowHints = rh,
                         colHints = ch}
    keychan <- gets keyChan

    liftIO $ forkIO $ do
        c <- newCollection
        fg <- newFocusGroup

        gridTbl <- toTable rows (replicate x $ column (ColFixed cellWidth)) BorderFull

        let rhx = fst (dimensions rh)
        rowHintsTbl <- toTable rh (replicate rhx $ column (ColFixed cellWidth)) BorderNone


        paddingCol <- plainText ""
        let chx = fst (dimensions ch)
            paddingColSpec = column $ ColFixed $ cellWidth * rhx
            normalColSpecs = replicate chx $ column (ColFixed $ cellWidth + 1)
            colHintsWithPaddedCol = map (paddingCol:) ch
        colHintsTbl <- toTable colHintsWithPaddedCol (paddingColSpec:normalColSpecs) BorderNone


        disp <- rowHintsTbl `hBox` gridTbl >>= (vBox colHintsTbl) >>= centered
        _ <- addToCollection c disp fg

        fg `onKeyPressed` \_ key modifiers -> do
          atomically $ writeTChan keychan (key, modifiers)
          return True

        runUi c defaultContext
    return ()

isDir key = key `elem` [KRight, KLeft, KUp, KDown]
dirForKey key = lookup key [(KRight, Rgt), (KLeft, Lft), (KUp, Up), (KDown, Down)]

uiLoop game = do
    keyChan <- gets keyChan
    redraw game
    (key, modifiers) <- liftIO $ atomically $ readTChan keyChan
    case (key, modifiers) of
      (KASCII 'q', _) -> return Quit

      (KASCII 'u', _) -> do clearMark
                            redraw game
                            return Undo

      (KASCII 'r', _) -> do clearMark
                            redraw game
                            return Redo

      (KASCII 'x', _) -> do coords <- selectedCoords
                            clearMark
                            redraw game
                            sq <- squareAtMark game
                            let sq' = if sq == Filled then Unknown else Filled
                            return $ Update sq' coords

      (KASCII ' ', _) -> do coords <- selectedCoords
                            clearMark
                            redraw game
                            return $ Update Unknown coords

      (KASCII 'c', _) -> do coords <- selectedCoords
                            clearMark
                            redraw game
                            sq <- squareAtMark game
                            let sq' = if sq == Empty then Unknown else Empty
                            return $ Update sq' coords

      _ | isDir key -> do let Just dir = dirForKey key
                          markSet <- markIsSet
                          aligned <- pointAlignedWithMark dir
                          let shifted = MShift `elem` modifiers

                          case () of
                            _ | not markSet && shifted -> setMarkAtPoint
                              | markSet && not shifted -> clearMark
                              | markSet && not aligned -> setPointAtMark
                              | otherwise              -> return ()

                          movePoint game dir
                          uiLoop game
      _ -> uiLoop game


instance UI VtyIO where
  type UIData VtyIO = VtyData

  initialize game = VtyIO $ do d <- emptyVtyData
                               execStateT (initializeM game) d

  display game vtydata = VtyIO $ return ()
  promptGuesses game vtydata = VtyIO $ runStateT (uiLoop game) vtydata

  shutdown vtyData = VtyIO $ do let d = done vtyData
                                schedule $ shutdownUi >> putMVar d ()
                                takeMVar d
