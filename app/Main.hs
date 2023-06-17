{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
module Main where

import Data.Bool
import Codec.BMP
import Data.Functor
import Data.Foldable
import Data.Function
import Graphics.Gloss
import Data.Traversable
import System.Directory
import Data.Map qualified as M
import Data.Vector qualified as V
import Graphics.Gloss.Interface.IO.Interact


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 7 .:


data Chessman
  = Pawn
  | Rook
  | Horsey
  | Bishop
  | Queen
  | King
  deriving (Eq, Ord)
data Piece = White Chessman | Black Chessman | Empty
  deriving (Eq, Ord)
type Board = V.Vector (V.Vector Piece)
data World = World
  { board     :: Board
  , selected  :: Maybe (Int, Int)
  , whiteTurn :: Bool
  }

q :: Float -- half the width of the board
q = 300

square :: Float -> Float -> Float -> Path
square x y w = [(x, y), (x+w, y), (x+w, y+w), (x, y+w), (x, y)]

drawBoard :: M.Map Piece Picture -> World -> Picture
drawBoard imgs world = Pictures . fold $ liftA2 draw [0..7] [0..7]
  where
    draw  x y = [tile, img] <&> \f -> f x y (coord x) (coord y)
    coord n   = fromIntegral n * (q / 4) - q

    getImg x y = case board world V.! y V.! x of
                   Empty -> Blank
                   p     -> imgs M.! p
    img    x y cX cY = Translate (cX + q/8) (cY + q/8) (getImg x y)
    tile   x y cX cY = colour x y . polygon $ square cX cY (q/4)
    colour = Color . bool white (light $ light blue) . odd . fromEnum .: (+)

border :: Picture
border = Line $ square -q -q (q*2)

convert :: M.Map Piece Picture -> World -> Picture
convert imgs world = Pictures [drawBoard imgs world, border]

getCoords :: (Float, Float) -> Maybe (Int, Int)
getCoords (mX, mY) = if valid x && valid y then Just (x, y) else Nothing
  where
    x = coordToSquare mX
    y = coordToSquare mY
    coordToSquare c = 4 + floor (c / (q / 4))
    valid s = 0 <= s && s < 8

get :: (Int, Int) -> V.Vector (V.Vector a) -> a
get (x, y) v = v V.! y V.! x

change :: (Int, Int) -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
change (x, y) a v = v V.// [(y, row)]
  where row = (v V.! y) V.// [(x, a)]

move :: (Int, Int) -> (Int, Int) -> Board -> Board
move s e board = change e piece $ change s Empty board
  where piece = board V.! snd s V.! fst s

events :: Event -> World -> World
events (EventKey (MouseButton LeftButton) Down _ mouse) world =
  case (selected world, getCoords mouse) of
    (s, Just c) -> case s of
                     Nothing -> case (piece, turn) of
                       (White _, True)  -> sel
                       (Black _, False) -> sel
                       (_,    _)        -> world
                       where sel = world { selected = Just c }
                     Just s -> if s == c then noS else noS { board = move s c b, whiteTurn = not turn }
                       where noS = world { selected = Nothing }
      where
        turn = whiteTurn world
        piece = b V.! snd c V.! fst c
        b = board world
    _                -> world
events _ world = world


home :: V.Vector Chessman
home = V.fromList [Rook, Horsey, Bishop, Queen, King, Bishop, Horsey, Rook]

start :: Board
start = V.fromList
  [ Black <$> home
  , V.replicate 8 $ Black Pawn
  , empty
  , empty
  , empty
  , empty
  , V.replicate 8 $ White Pawn
  , White <$> home
  ]
  where empty = V.replicate 8 Empty

imgNames :: [String]
imgNames = liftA2 (\p c -> [p, c]) "kqrnbp" "dl"

pieceOrder :: [Piece]
pieceOrder = liftA2 (&) [King, Queen, Rook, Horsey, Bishop, Pawn] [Black, White]


main :: IO ()
main = do
  path <- (<> "/imgs/") <$> getCurrentDirectory
  imgs <- for imgNames $ \name -> loadBMP $ path <> name <> ".bmp"
  Right bmp <- readBMP $ path <> "kd.bmp"

  let factor     = q/4 / (fromIntegral . fst $ bmpDimensions bmp) -- it's a square.
      scaledImgs = Scale factor factor <$> imgs
      window     = InWindow "caos9df8gukdsasd9hfjlksas!!!" (200, 200) (10, 10)
      world      = World { board = start, selected = Nothing, whiteTurn = True }

  play window white 0 world (convert . M.fromList $ zip pieceOrder scaledImgs) events $ flip const
