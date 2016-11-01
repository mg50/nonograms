{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving #-}
module UI.Vty where

import qualified Brick.Main as BM
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Core as BC
import qualified Brick.Util as BU
import Brick.Widgets.Core ((<+>), (<=>))
import qualified Brick.Widgets.Center as Center
import Brick.Types (Widget, EventM, Next, Padding(..))
import qualified Graphics.Vty as Vty

import Core
import Data.Default
import qualified Data.List as L
import UI
import Action
import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

data DisplayData = DisplayData { point :: (Int, Int)
                               , mark :: Maybe (Int, Int)
                               , displaySolution :: Bool
                               }

data VtyChan = VtyChan { eventChan :: Chan.Chan NonoEvent
                       , actionResult :: MVar Action
                       , done :: MVar ()
                       }

type DisplayM a = State DisplayData a

newtype VtyIO a = VtyIO { unVtyIO :: IO a } deriving (Functor, Applicative, Monad)

data NonoEvent = VtyEvent Vty.Event | MakeMove Game | Stop

data Direction = Up | Down | Lft | Rgt

coordsForDirection dir = case dir of
  Up   -> (0, -1)
  Down -> (0, 1)
  Lft  -> (-1, 0)
  Rgt  -> (1, 0)

movePoint :: Game -> Direction -> DisplayM ()
movePoint game dir = let (dimX, dimY) = dimensions (current game)
                         (dx, dy) = coordsForDirection dir
                     in modify $ \cursor -> do
                       let (x, y) = point cursor
                           x' = x + dx
                           y' = y + dy
                       if x' `elem` [0..dimX-1] && y' `elem` [0..dimY-1]
                          then cursor{point=(x', y')}
                          else cursor

clearMark :: DisplayM ()
clearMark = modify $ \cursor -> cursor{mark=Nothing}

markIsSet :: DisplayM Bool
markIsSet = do m <- gets mark
               case m of
                 Nothing -> return False
                 _       -> return True

setMarkAtPoint :: DisplayM ()
setMarkAtPoint = modify $ \cursor -> let pt = (point cursor)
                                     in cursor{mark=(Just pt)}

pointAlignedWithMark :: Direction -> DisplayM Bool
pointAlignedWithMark dir = do (px, py) <- gets point
                              let (dx, dy) = coordsForDirection dir
                                  px' = px + dx
                                  py' = py + dy
                              mk <- gets mark
                              return $ case mk of
                                         Nothing -> False
                                         Just (mx, my) | px' == mx  -> True
                                                       | py' == my  -> True
                                                       | otherwise  -> False

setPointAtMark :: DisplayM ()
setPointAtMark = do mk <- gets mark
                    case mk of
                      Nothing -> return ()
                      Just m -> modify $ \info -> info{point = m}

squareAtPoint :: Game -> DisplayM Square
squareAtPoint game = do (x, y) <- gets point
                        return $ squareAt game x y

cellWidth = 6
cellHeight = 3

charFor Filled = '#'
charFor Empty = '.'
charFor Unknown = ' '


to :: Int -> Int -> [Int]
to x y = if null [x..y] then [y..x] else [x..y]

type Point = (Int, Int)
between :: Point -> (Point, Point) -> Bool
between (x, y) ((a1, b1), (a2, b2)) | x == a1 && x == a2 && elem y (b1 `to` b2) = True
                                    | y == b1 && y == b2 && elem x (a1 `to` a2) = True
                                    | otherwise                                 = False

selectedCoords :: DisplayM [(Int, Int)]
selectedCoords = do (px, py) <- gets point
                    mk <- gets mark
                    case mk of
                      Nothing -> return [(px, py)]
                      Just (mx, my) | px == mx -> return $ map (\n -> (px, n)) $ py `to` my
                                    | py == my -> return $ map (\n -> (n, py)) $ px `to` mx

squareText :: Square -> String
squareText square = charFor square |> replicate cellWidth
                                   |> replicate cellHeight
                                   |> L.intercalate "\n"

emptyVtyChan :: IO VtyChan
emptyVtyChan = do eventChan <- Chan.newChan
                  actionResult <- newEmptyMVar
                  done <- newEmptyMVar
                  return $ VtyChan eventChan actionResult done

emptyDisplayData :: DisplayData
emptyDisplayData = DisplayData (0, 0) Nothing False

pointAttr = BA.attrName "point"
markedAttr = BA.attrName "marked"
provenAttr = BA.attrName "proven"
unprovenAttr = BA.attrName "unproven"

attrMap = BA.attrMap Vty.defAttr [ (pointAttr, Vty.brightWhite `BU.on` Vty.blue)
                                 , (markedAttr, Vty.brightWhite `BU.on` Vty.brightBlue)
                                 , (provenAttr, BU.fg Vty.brightWhite)
                                 , (unprovenAttr, BU.fg Vty.white)
                                 ]

padList :: a -> [[a]] -> [[a]]
padList _ [] = []
padList x xss = let l = maximum (map length xss)
                in flip map xss $ \xs -> let n = length xs
                                         in replicate (l-n) x ++ xs

for = flip map

attrForProven True = provenAttr
attrForProven False = unprovenAttr

makeRowHints :: Game -> (Int, Widget ())
makeRowHints game =
  let rowHints = hints (solution game) (current game)

      hintRowSeparation = 1

      rowLength row = row |> map fst
                          |> map show
                          |> L.intercalate " "
                          |> (\str -> " " ++ str ++ " ")
                          |> length

      maxRowLength = rowHints |> map rowLength
                              |> foldr max 0

      vPadding = cellHeight `div` 2
      abovePadding = replicate (vPadding + 1) (BC.str " ") |> foldr (<=>) (BC.str "")
      belowPadding = replicate vPadding (BC.str " ") |> foldr (<=>) (BC.str "")

      makeRow :: [Widget ()] -> Widget ()
      makeRow row = row |> (\r -> case r of
                                    [] -> (BC.str " ")
                                    _  -> foldl (\w row -> w <+> row) BC.emptyWidget r)
                        |> (\r -> abovePadding <=> r <=> belowPadding)
                        |> BC.padLeft Max
                        |> BC.hLimit maxRowLength
                        |> (<+> BC.str (replicate hintRowSeparation ' '))

      makeCol :: [Widget ()] -> Widget ()
      makeCol = foldl (\w col -> w <=>  col) (BC.str "")

      widget (n, proven) = BC.withAttr (attrForProven proven) $
                             BC.str (" " ++ show n)

      widgets = for rowHints $ \row ->
                  for row $ \hint ->
                    widget hint

  in (maxRowLength, makeCol $ map makeRow widgets)

makeColHints :: Game -> Widget ()
makeColHints game =
  let transposed = transpose game
      colHints = hints (solution transposed) (current transposed)

      maxColLength = colHints |> map length |> foldr max 0 |> (+1)

      hPadding = cellWidth `div` 2
      leftPadding = replicate (hPadding) (BC.str " ") |> foldr (<+>) (BC.str "")
      rightPadding = replicate hPadding (BC.str " ") |> foldr (<+>) (BC.str "")

      makeCol :: [Widget ()] -> Widget ()
      makeCol col = col |> foldl (\w col -> w <=>  col) (BC.str " ")
                        |> (\c -> leftPadding <+> c <+> rightPadding)
                        |> BC.padTop Max
                        |> BC.vLimit maxColLength

      widget (n, proven) = BC.withAttr (attrForProven proven) $ BC.str (show n)

      widgets = for colHints $ \col ->
                  for col $ \hint ->
                    widget hint

      widgetColumns = map makeCol widgets

  in foldl (<+>) (BC.str "") widgetColumns

makeCell :: DisplayData -> Square -> Int -> Int -> Widget ()
makeCell displayData square x y =
  let setBgColor w = case mark displayData of
        _ | (x, y) == point displayData -> BC.withAttr pointAttr w
        Just mk | (x, y) `between` (point displayData, mk) -> BC.withAttr markedAttr w
        _ -> w

  in setBgColor $ BC.str (squareText square)

drawUi :: (Game, DisplayData, VtyChan) -> [Widget ()]
drawUi (game, displayData, vtyChan) =
  let nono = if displaySolution displayData
                then solution game
                else current game
      (numCols, numRows) = dimensions nono

      makeRow :: [Widget ()] -> Widget ()
      makeRow = BC.vLimit cellHeight . foldl1 (\w row -> w <+> BB.vBorder <+> row)

      makeCol :: [Widget ()] -> Widget ()
      makeCol = foldl1 (\w col -> w <=> BB.hBorder <=> col)

      cells = for (rows nono `zip` [0..]) $ \(row, y) ->
                for (row `zip` [0..]) $ \(square, x) ->
                  makeCell displayData square x y

      grid = BC.withBorderStyle BBS.unicode $
        BB.border $
        BC.hLimit (numCols * (cellWidth + 1) - 1) $
        BC.vLimit (numRows * (cellHeight + 1) - 1) $
        makeCol (map makeRow cells)

      (rowHintsPadding, rowHints) = makeRowHints game
      colHints = BC.padLeft (Pad (rowHintsPadding + 1)) $ makeColHints game

  in [Center.center $ BC.vBox [colHints, BC.hBox [rowHints, grid]]]

dirKeyMap :: [(Vty.Key, Direction)]
dirKeyMap =  [(Vty.KRight, Rgt), (Vty.KLeft, Lft), (Vty.KUp, Up), (Vty.KDown, Down)]

appEvent :: (Game, DisplayData, VtyChan) -> NonoEvent -> EventM () (Next (Game, DisplayData, VtyChan))
appEvent state Stop = BM.halt state

appEvent (_, displayData, vtyChan) (MakeMove newGame) = BM.continue (newGame, displayData, vtyChan)

appEvent state@(game, displayData, vtyChan) (VtyEvent (Vty.EvKey key modifiers)) = do
  let putResult = liftIO . putMVar (actionResult vtyChan)
      continue = BM.continue state
      quit = putResult Quit >> continue

  case key of
    Vty.KChar 'q' -> quit

    Vty.KChar 'd' -> if Vty.MCtrl `elem` modifiers
                        then quit
                        else continue

    Vty.KChar 'u' -> do let newData = execState clearMark displayData
                        putResult Undo
                        BM.continue (game, newData, vtyChan)

    Vty.KChar 'r' -> do let newData = execState clearMark displayData
                        putResult Redo
                        BM.continue (game, newData, vtyChan)

    Vty.KChar 'x' -> do let action = do coords <- selectedCoords
                                        clearMark
                                        sq <- squareAtPoint game
                                        let sq' = if sq == Filled then Unknown else Filled
                                        return (sq', coords)
                            ((sq, coords), newData) = runState action displayData

                        putResult (Update sq coords)
                        BM.continue (game, newData, vtyChan)

    Vty.KChar 'c' -> if Vty.MCtrl `elem` modifiers
                        then quit
                        else do let action = do coords <- selectedCoords
                                                clearMark
                                                sq <- squareAtPoint game
                                                let sq' = if sq == Empty then Unknown else Empty
                                                return (sq', coords)
                                    ((sq, coords), newData) = runState action displayData

                                putResult (Update sq coords)
                                BM.continue (game, newData, vtyChan)

    Vty.KChar ' ' -> do let action = do coords <- selectedCoords
                                        clearMark
                                        return coords
                            (coords, newData) = runState action displayData

                        putResult (Update Unknown coords)
                        BM.continue (game, newData, vtyChan)

    Vty.KChar 's' -> let newData = displayData{displaySolution = not (displaySolution displayData)}
                     in BM.continue (game, newData, vtyChan)

    key ->
      case lookup key dirKeyMap of
        Nothing -> continue
        Just dir -> let action = do markSet <- markIsSet
                                    aligned <- pointAlignedWithMark dir
                                    let shifted = Vty.MShift `elem` modifiers

                                    case () of
                                      _ | not markSet && shifted -> setMarkAtPoint
                                        | markSet && not shifted -> clearMark
                                        | markSet && not aligned -> setPointAtMark
                                        | otherwise              -> return ()

                                    movePoint game dir
                        newData = execState action displayData

                    in BM.continue (game, newData, vtyChan)

app :: BM.App (Game, DisplayData, VtyChan) NonoEvent ()
app = BM.App { BM.appDraw = drawUi
             , BM.appAttrMap = const attrMap
             , BM.appLiftVtyEvent = VtyEvent
             , BM.appChooseCursor = BM.neverShowCursor
             , BM.appHandleEvent = appEvent
             , BM.appStartEvent = return
             }

instance UI VtyIO where
  type UIChannel VtyIO = VtyChan

  initialize game = VtyIO $ do
    vtyChan <- emptyVtyChan
    forkIO $ do let initialState = (game, emptyDisplayData, vtyChan)
                BM.customMain (Vty.mkVty def) (eventChan vtyChan) app initialState
                putMVar (done vtyChan) ()
    return vtyChan

  display game vtyChan = VtyIO $ Chan.writeChan (eventChan vtyChan) (MakeMove game)

  promptAction game vtyChan = VtyIO $ takeMVar (actionResult vtyChan)

  shutdown vtyChan = VtyIO $ do Chan.writeChan (eventChan vtyChan) Stop
                                takeMVar (done vtyChan)
