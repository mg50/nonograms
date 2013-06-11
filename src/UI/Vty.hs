{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module UI.Vty where
import Graphics.Vty.Widgets.All
import Graphics.Vty hiding (Cursor)
import Graphics.Vty.Attributes
import Core
import qualified Data.List as L
import qualified Data.Text as T
import UI
import System.Exit

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State


data CellUIType = Plain | Point | Marked deriving (Show)
data CellUI = CellUI { square :: Square, cellUIType :: CellUIType }

data VtyData = VtyData { cells :: [[Widget FormattedText]]
                       , point :: (Int, Int)
                       , mark :: Maybe (Int, Int)
                       , selectionChan :: TChan [[Widget FormattedText]]
                       , keyChan :: TChan Key }
newtype VtyM a = VtyM { runVtyM :: StateT VtyData IO a } deriving (Monad)

instance MonadIO VtyM where
  liftIO = VtyM . lift

data Direction = Up | Down | Lft | Rgt
coordsForDirection dir = case dir of
  Up   -> (0, -1)
  Down -> (0, 1)
  Lft  -> (-1, 0)
  Rgt  -> (1, 0)

movePoint :: Game -> Direction -> VtyM ()
movePoint game dir = let dims = dimensions (current game)
                         (dx, dy) = coordsForDirection dir
                     in VtyM $ modify $ \cursor -> do
                       let (x, y) = point cursor
                       let x' = x + dx
                       let y' = y + dy
                       if x' `elem` [0..x-1] && y' `elem` [0..y-1]
                          then cursor{point=(x', y')}
                          else cursor

clearMark :: VtyM ()
clearMark = VtyM $ modify $ \cursor -> cursor{mark=Nothing}

setMarkAtPoint :: VtyM ()
setMarkAtPoint = VtyM $ modify $ \cursor -> let pt = (point cursor)
                                               in cursor{mark=(Just pt)}

setCells :: [[Widget FormattedText]] -> VtyM ()
setCells cells = VtyM $ modify $ \vty -> vty{cells=cells}


cellWidth = 3
cellHeight = 2

charFor Filled = '#'
charFor Empty = 'X'
charFor Unknown = 'A'

type Point = (Int, Int)
between :: Point -> (Point, Point) -> Bool
between (x, y) ((a1, b1), (a2, b2)) | x == a1 && x == a2 && elem y [b1..b2] = True
                                    | y == b1 && y == b2 && elem x [a1..a2] = True
                                    | otherwise                             = False


getUIType :: (Int, Int) -> VtyM CellUIType
getUIType coords = VtyM $ do
  pt <- gets point
  mk <- gets mark
  return $ case mk of
             _ | pt == coords -> Point
             Just m | coords `between` (m, pt) -> Marked
             _ -> Plain

squareText square = charFor square |> replicate cellWidth
                                   |> replicate cellHeight
                                   |> L.intercalate "\n"
                                   |> T.pack

cellStyle Plain  = Attr Default Default Default
cellStyle Point  = Attr Default (SetTo black) (SetTo white)
cellStyle Marked = Attr Default (SetTo black) (SetTo bright_blue)

createCells :: Int -> Int -> IO [[Widget FormattedText]]
createCells x y = replicateM y $ replicateM x $ plainText ""

formatGrid :: Game -> VtyM ()
formatGrid game = VtyM $ do c <- gets cells
                            let nono = current game
                            forM_ (c `zip` [0..]) $ \(row, rowNum) -> do
                              forM_ (row `zip` [0..]) $ \(widget, colNum) -> do
                                uiType <- getUIType (colNum, rowNum)
                                let style = cellStyle uiType
                                let txt = squareText $ squareAt nono (colNum, rowNum)
                                liftIO $ setTextWithAttrs widget [(txt, style)]


emptyVty :: IO Vty
emptyVty = do keyCh <- newTChanIO
              selectionCh <- newTChanIO
              return $ Vty [] (0, 0) Nothing keyCh selectionCh

initialize game = do let nono = solution game
                     let (x, y) = dimensions nono
                     rows <- createCells x y
                     modify $ \vty -> vty{rows = rows}
                     tableRows <- forM rows $ \row -> row |> map mkRow
                                                          |> foldl1 (.|.)
                                                          |> return

                     ch <- gets keyChan

                     liftIO $ do c <- newCollection
                                 fg <- newFocusGroup
                                 tbl <- newTable (replicate x $ column (ColFixed cellWidth)) BorderFull
                                 sequence_ $ map (addRow tbl) tableRows
                                 addToCollection c tbl fg

                                 fg `onKeyPressed` \_ key _ -> do
                                   atomically $ writeTChan ch key
                                   return True

                                 runUi c defaultContext
                     uiLoop ch

uiLoop ch = do key <- liftIO $ atomically $ readTChan ch
               case key of
                 KASCII 'q' -> liftIO shutdownUi
--                 KRight ->

               uiLoop ch



-- instance UI VtyM where
--   display game = return ()
--   initialize game = do let nono = solution game
--                        let (x, y) = dimensions nono
--                        rows <- forM [0..(y - 1)] $ (drawRow game)
--                        setCells rows
--                        tableRows <- forM rows $ \row -> row |> map mkRow
--                                                             |> foldl1 (.|.)
--                                                             |> return

--                        ch <- liftIO newTChanIO

--                        liftIO $ do c <- newCollection
--                                    fg <- newFocusGroup
--                                    tbl <- newTable (replicate x $ column (ColFixed cellWidth)) BorderFull
--                                    sequence_ $ map (addRow tbl) tableRows
--                                    addToCollection c tbl fg

--                                    fg `onKeyPressed` \_ key _ -> do
--                                      atomically $ writeTChan ch key
--                                      return True

--                                    runUi c defaultContext

--                        key <- liftIO $ atomically $ readTChan ch
--                        case key of
--                          KASCII 'q' -> shutdownUi
--                          _ -> setText (rows !! 0 !! 0) (T.pack "awef")
--                          -- _ -> forM_ rows $ \row ->
--                          --        forM_ row $ \txt ->
--                          --          setText txt (T.pack "fff\nfff")

--                        return ()
--   promptGuesses game = undefined






-- -- main :: IO ()
-- -- main = do
-- --   e <- editWidget
-- --   ui <- centered e
-- --   fg <- newFocusGroup


-- --   t <- plainText $ T.pack "hi"

-- --   -- let columnSize = ColFixed 3
-- --   -- let spec = ColumnSpec columnSize Nothing Nothing
-- --   -- t <- newTable [spec] BorderFull



-- --   addToFocusGroup fg t
-- --   c <- newCollection

-- --   b1 <- (plainText "foo\naowiejf") <--> hBorder <--> (plainText "bar")
-- --   b2 <- (return b1) <++> vBorder <++> (plainText "baz")
-- --   b3 <- bordered b2

-- --   addToCollection c b3 fg
-- --   e `onActivate` \this ->
-- --     getEditText this >>= (error . ("You entered: " ++) . T.unpack)
-- --   runUi c defaultContext
