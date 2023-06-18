{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.Bool
import Codec.BMP
import Data.Tuple
import Data.Functor
import Data.Foldable
import Control.Monad
import Data.Function
import Lens.Micro.Mtl
import Graphics.Gloss
import Data.Traversable
import System.Directory
import Control.Monad.State
import Data.Generics.Labels()
import GHC.Generics (Generic)
import Data.Map qualified as M
import Math.Geometry.GridMap.Lazy
import Math.Geometry.Grid.Octagonal
import Graphics.Gloss.Interface.IO.Interact
import Math.Geometry.GridMap hiding (filter)


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 7 .:


data Colour = White | Black deriving (Eq, Ord)
other :: Colour -> Colour
other Black = White
other White = Black

data Chessman
  = Pawn
  | Rook
  | Horsey
  | Bishop
  | Queen
  | King
  deriving (Eq, Ord)
data Piece = Piece Colour Chessman | Empty
  deriving (Eq, Ord)
type Board = LGridMap RectOctGrid Piece
data World = World
  { board    :: Board
  , selected :: Maybe (Int, Int)
  , lastMove :: Maybe (Int, Int)
  , turn     :: Colour
  }
  deriving Generic

q :: Float -- half the width of the board
q = 300

square :: Float -> Float -> Float -> Path
square x y w = [(x, y), (x+w, y), (x+w, y+w), (x, y+w), (x, y)]

drawBoard :: M.Map Piece Picture -> World -> Picture
drawBoard imgs world = Pictures . fold $ liftA2 draw [0..7] [0..7]
  where
    draw  x y = [tile, img] <&> \f -> f x y (coord x) (coord y)
    coord n   = fromIntegral n * (q / 4) - q

    getImg x y = case world.board ! (y, x) of
                   Empty -> Blank
                   p     -> imgs M.! p
    baseImg x y = (if Just (x, y) == world.selected then Scale 1.2 1.2 else id) $ getImg x y
    img     x y cX cY =  Translate (cX + q/8) (cY + q/8) $ baseImg x y
    tile    x y cX cY = colour x y . polygon $ square cX cY (q/4)
    colour  x y = Color . (if Just (x, y) == world.lastMove then dark else id) $ baseColour x y
    baseColour = bool white (light $ light blue) . odd . fromEnum .: (+)

border :: Picture
border = Line $ square -q -q (q*2)

convert :: M.Map Piece Picture -> World -> Picture
convert imgs world = Pictures [drawBoard imgs world, border]


toSquare :: Float -> Maybe Int
toSquare s = if elem c [0..7] then Just c else Nothing
  where c = 4 + floor (s / (q / 4))

move :: (Int, Int) -> (Int, Int) -> Board -> Board
move s e board = insert (swap e) (board ! swap s) $ insert (swap s) Empty board

validMoves :: (Int, Int) -> Board -> [(Int, Int)]
validMoves (x, y) b =
  let Piece colour man = b ! (y, x)
      isCol c piece = case piece of
        Piece mColour _ | mColour == c -> True
        _ -> False
      good c@(mX, mY) = elem mX [0..7] && elem mY [0..7] && (not . isCol colour $ b ! swap c) in

  filter good $ case man of
    Horsey -> [(mX+x, mY+y) | mX <- [-2..2], mY <- [-2..2], (abs $ mX*mY) == 2]
    King   -> [(mX+x, mY+y) | mX <- [-1..1], mY <- [-1..1], mX /= 0 || mY /= 0]
    Pawn   -> [p | (p, f) <- cases, f $ b ! swap p]
      where
        cases = [ ((x,   y+dir),   (Empty ==))
                , ((x,   y+dir*2), (starting &&) . (Empty ==))
                , ((x+1, y+dir),   isCol $ other colour)
                , ((x-1, y+dir),   isCol $ other colour)
                ]
        dir = if colour == Black then 1 else -1
        starting = y == case colour of
            White -> 6
            Black -> 1
    _ -> []

events :: Event -> State World () -- todo: piece piece = select
events (EventKey (MouseButton LeftButton) keyState _ (toSquare -> Just x, toSquare -> Just y)) = do
  World board selected _ turn <- get
  let click = (x, y)
  case (keyState, selected) of
    (Down, Nothing) -> case board ! (y, x) of
      Piece colour _ | colour == turn -> #selected ?= click
      _ -> pure ()
    (Down, Just selection)                      -> moveIfValid board click selection
    (Up,   Just selection) | click /= selection -> moveIfValid board click selection
    _ -> pure ()
  where
    moveIfValid board click selection = do
      #selected .= Nothing
      when (elem click $ validMoves selection board) do
        #board    %= move selection click
        #lastMove ?= click
        #turn     %= other
events _ = pure ()


home :: [Chessman]
home = [Rook, Horsey, Bishop, Queen, King, Bishop, Horsey, Rook]

start :: Board
start = lazyGridMap (rectOctGrid 8 8) $
  fold [ Piece Black <$> home
       , replicate 8 $ Piece Black Pawn
       , replicate 32 Empty
       , replicate 8 $ Piece White Pawn
       , Piece White <$> home
       ]

imgNames :: [String]
imgNames = liftA2 (\p c -> [p, c]) "kqrnbp" "dl"

pieceOrder :: [Piece]
pieceOrder = liftA2 (&) [King, Queen, Rook, Horsey, Bishop, Pawn] [Piece Black, Piece White]


main :: IO ()
main = do
  path <- getCurrentDirectory
  imgs <- for imgNames $ \name -> loadBMP $ fold [path, "/imgs/", name, ".bmp"]
  Right bmp <- readBMP $ path <> "/imgs/kd.bmp"

  let factor     = q/4 / (fromIntegral . fst $ bmpDimensions bmp) -- it's a square.
      scaledImgs = Scale factor factor <$> imgs
      w          = round $ q*2
      window     = InWindow "caos9df8gukdsasd9hfjlksas!!!" (w, w) (10, 10)
      world      = World { board = start, selected = Nothing, lastMove = Nothing, turn = White }

  play window white 0 world (convert . M.fromList $ zip pieceOrder scaledImgs) (execState . events) $ flip const
