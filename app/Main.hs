{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
module Main where

import Data.Bool
import Codec.BMP
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


data Chessman = Pawn
              | Rook
              | Horsey
              | Bishop
              | Queen
              | King
  deriving (Eq, Ord)
data Piece = White Chessman | Black Chessman | Empty
  deriving (Eq, Ord)

--instance Eq Piece where
--  Empty   == Empty    =  True
--  White x == White y  =  x == y
--  Black x == Black y  =  x == y
--instance Ord Piece where
--  compare (White x) (White y) = compare x y
--  compare (White _) _ = GT
--  compare (Black x) (Black y) = compare x y
--  compare (Black kk
--  compare Empty Empty = EQ
--  compare _ _ = EQ

type Board = V.Vector (V.Vector Piece)

q :: Float -- half the width of the board
q = 300

square :: Float -> Float -> Float -> Path
square x y w = [(x, y), (x+w, y), (x+w, y+w), (x, y+w), (x, y)]

drawBoard :: M.Map Piece Picture -> Board -> Picture
drawBoard imgs board = Pictures $ liftA2 draw [0..7] [0..7]
  where
    draw :: Int -> Int -> Picture
    draw   x y        = Pictures $ (\f -> f x y (coord x) (coord y)) <$> [tile, img]
    coord             = (- q) . (* (q/4)) . fromIntegral
    img    x y cX cY  = Translate (cX + q/8) (cY + q/8) $ getImg x y
    getImg x y        = case board V.! y V.! x of
                          Empty -> Pictures []
                          p     -> imgs M.! p
    tile   x y cX cY  = colour x y . polygon $ square cX cY (q/4)
    colour            = Color . bool white (light $ light blue) . odd . fromEnum .: (+)

border :: Picture
border = Line $ square -q -q (q*2)

convert :: M.Map Piece Picture -> Board -> Picture
convert imgs board = Pictures [drawBoard imgs board, border]

events :: Event -> Board -> Board
events _ = id

step :: Float -> Board -> Board
step _ = id

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

  let factor = q/4 / (fromIntegral . fst $ bmpDimensions bmp) -- it's a square.
      scaledImgs = Scale factor factor <$> imgs
      window = InWindow "caos9df8gukdsasd9hfjlksas!!!" (200, 200) (10, 10)

  play window white 0 start (convert . M.fromList $ zip pieceOrder scaledImgs) events step
