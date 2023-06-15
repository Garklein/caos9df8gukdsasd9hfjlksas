{-# LANGUAGE LexicalNegation #-}
module Main where

import Data.Bool
import Data.List
import Codec.BMP
import Data.Maybe
import Graphics.Gloss
import System.Directory
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
letter :: Chessman -> String
letter Pawn   = "p"
letter Rook   = "r"
letter Horsey = "n"
letter Bishop = "b"
letter Queen  = "q"
letter King   = "k"

data Piece = White Chessman | Black Chessman | Empty
pieceName :: Piece -> String
pieceName (Black x) = letter x ++ "d"
pieceName (White x) = letter x ++ "l"
pieceName Empty     = ""

type Board = V.Vector (V.Vector Piece)

imgNames :: [String]
imgNames = pieces "d" ++ pieces "l"
  where
    pieces c = (++ c) <$> ["k", "q", "r", "n", "b", "p"]



q :: Float -- half the width of the board
q = 300

square :: Float -> Float -> Float -> Path
square x y w = [(x, y), (x+w, y), (x+w, y+w), (x, y+w), (x, y)]

drawBoard :: [Picture] -> Board -> Picture
drawBoard imgs board = Pictures $ liftA2 all [0..7] [0..7]
  where
    all :: Int -> Int -> Picture
    all    x y        = Pictures $ (\f -> f x y (fromIntegral x * w - q) (fromIntegral y * w - q)) <$> [tile, img]
    img    x y cX cY  = Translate (cX + w/2) (cY + w/2) $ getImg x y
    getImg x y        = case board V.! y V.! x of
                          Empty -> Pictures []
                          p     -> imgs !! fromJust (elemIndex (pieceName p) imgNames)
    tile   x y cX cY  = colour x y . polygon $ square cX cY w
    colour            = Color . bool white (light $ light blue) . odd . fromEnum .: (+)
    w                 = q / 4

border :: Picture
border = Line $ square -q -q (q*2)

convert :: [Picture] -> Board -> Picture
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
  , gimme8 $ Black Pawn
  , gimme8 Empty
  , gimme8 Empty
  , gimme8 Empty
  , gimme8 Empty
  , gimme8 $ White Pawn
  , White <$> home
  ]
  where gimme8 = V.replicate 8

main :: IO ()
main = do
  path <- (++ "/imgs/") <$> getCurrentDirectory
  let getImg name = loadBMP $ path ++ name ++ ".bmp"
  imgs <- traverse getImg imgNames

  Right bmp <- readBMP $ path ++ "kd.bmp"
  let factor = (q / 4 /) . fromIntegral . fst $ bmpDimensions bmp -- it's a square.
  let scaledImgs = (Scale factor factor) <$> imgs

  let window = InWindow "caos9df8gukdsasd9hfjlksas!!!" (200, 200) (10, 10)
  play window white 0 start (convert scaledImgs) events step
