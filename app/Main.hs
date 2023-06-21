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
import Data.Maybe
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
import Prelude hiding (lookup)
import Data.Map qualified as M
import Math.Geometry.GridMap.Lazy
import Math.Geometry.Grid.Octagonal
import Math.Geometry.Grid hiding (null)
import Data.List hiding (insert, lookup)
import Graphics.Gloss.Interface.IO.Interact
import Math.Geometry.GridMap hiding (filter)


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 7 .:


data Colour = White | Black deriving (Eq, Ord)
other :: Colour -> Colour
other Black = White
other White = Black

type Board = LGridMap RectOctGrid Piece
data GameState = Ongoing | WhiteWin | BlackWin | Draw deriving Eq
colourToWin :: Colour -> GameState
colourToWin Black = BlackWin
colourToWin White = WhiteWin

data Chessman
  = Pawn
  | Rook
  | Horsey
  | Bishop
  | Queen
  | King
  deriving (Eq, Ord)
data Piece = Piece { nMoves :: Int, colour :: Colour, man :: Chessman } | Empty
  deriving (Eq, Ord)
data World = World
  { board     :: Board
  , selected  :: Maybe (Int, Int)
  , lastMove  :: Maybe (Int, Int)
  , turn      :: Colour
  , dims      :: (Int, Int)
  , gameState :: GameState
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
                   p     -> imgs M.! (p { nMoves = 0 })
    baseImg x y = (if Just (x, y) == world.selected then Scale 1.2 1.2 else id) $ getImg x y
    img     x y cX cY = Translate (cX + q/8) (cY + q/8) $ baseImg x y
    tile    x y cX cY = colour x y . Polygon $ square cX cY (q/4)
    colour  x y = Color . (if Just (x, y) == world.lastMove then dark else id) $ baseColour x y
    baseColour = bool (light $ light blue) white . odd . fromEnum .: (+)

border :: Picture
border = Line $ square -q -q (q*2)

background :: (Int, Int) -> Color -> Picture
background (w, h) c = Color c . Polygon $ square x y width
  where
    x     = fromIntegral -w / 2
    y     = fromIntegral -h / 2
    width = fromIntegral $ max w h

-- todo: make this change with window sizes
convert :: M.Map Piece Picture -> World -> Picture
convert imgs world = Pictures $ case world.gameState of
  Ongoing  -> [background world.dims $ if world.turn == White then white else black, drawBoard imgs world, border]
  WhiteWin -> [background world.dims white,       say black "White wins!"]
  BlackWin -> [background world.dims black,       say white "Black wins!"]
  Draw     -> [background world.dims $ greyN 0.8, say black "It's a draw!"]
  where
    say c s = Color c . Translate -400 0 $ Text s


toSquare :: Float -> Maybe Int
toSquare s = if elem c [0..7] then Just c else Nothing
  where c = 4 + floor (s / (q / 4))

move :: (Int, Int) -> (Int, Int) -> Board -> Board
move s e board = insert (swap e) (piece { nMoves = piece.nMoves + 1 }) $ insert (swap s) Empty board
  where
    piece = board ! swap s

isCol :: Colour -> Piece -> Bool
isCol c = \case
  Piece _ pC _ | pC == c -> True
  _ -> False

notCol :: Board -> Colour -> (Int, Int) -> Bool
notCol board colour (x, y) = not . isCol colour $ board ! (y, x)

type GetMoves = (Int, Int) -> Colour -> Board -> [(Int, Int)]

pawnMoves :: GetMoves
pawnMoves (x, y) colour board = [p | (p, f) <- cases, maybe False f $ lookup (swap p) board]
  where
    cases = [ ((x,   y+dir),   (Empty ==))
            , ((x,   y+dir*2), (nMoves == 0 &&) . (Empty ==))
            , ((x+1, y+dir),   isCol $ other colour)
            , ((x-1, y+dir),   isCol $ other colour)
            ]
    dir = if colour == Black then 1 else -1
    Piece nMoves _ _ = board ! (y, x)

kingMoves :: GetMoves
kingMoves (x, y) colour board = filter (notCol board colour) $ neighbours board (x, y)

horseyMoves :: GetMoves
horseyMoves (x, y) colour board = filter (\c -> contains board c && notCol board colour c) $
  [(mX+x, mY+y) | mX <- [-2..2], mY <- [-2..2], (abs $ mX*mY) == 2]

lineMoves :: (Int, Int) -> Colour -> Board -> (Int, Int) -> [(Int, Int)]
lineMoves (x, y) colour board (cX, cY) =
  take (min noSame tillTake) toEdge
  where
    noSame   = length $ takeWhile (notCol board colour) toEdge
    tillTake = 1 + (length $ takeWhile (notCol board (other colour)) toEdge)
    toEdge   = takeWhile (contains board) . tail $ zip [x, x+cX..] [y, y+cY..]

linesMoves :: [(Int, Int)] -> (Int, Int) -> Colour -> Board -> [(Int, Int)]
linesMoves dirs coords colour board = fold $ lineMoves coords colour board <$> dirs

rookMoves :: GetMoves
rookMoves = linesMoves [(1, 0), (-1, 0), (0, 1), (0, -1)]

bishopMoves :: GetMoves
bishopMoves = linesMoves [(1, 1), (-1, 1), (-1, -1), (1, -1)]

queenMoves :: GetMoves
queenMoves = linesMoves . filter (/= (0, 0)) $ liftA2 (,) [-1..1] [-1..1]

side :: Colour -> Board -> [(Int, Int)]
side colour board = [coords | coords <- indices board, isCol colour $ board ! swap coords]

threatening :: Colour -> Board -> [(Int, Int)]
threatening colour board = nub . fold $ rawMoves board <$> side colour board

check :: Colour -> Board -> Bool
check colour board = elem kingIdx $ threatening (other colour) board
  where
    kingIdx = fromJust . find isKing $ indices board
    isKing coords = case (board ! swap coords) of
      Piece { colour = c, man = King } | c == colour -> True
      _ -> False

-- moves without checking if they would reveal check
rawMoves :: Board -> (Int, Int) -> [(Int, Int)]
rawMoves board (x, y) =
  pieceMoves (x, y) colour board
  where
    Piece _ colour man = board ! (y, x)
    pieceMoves = case man of
      Horsey -> horseyMoves
      King   -> kingMoves
      Pawn   -> pawnMoves
      Rook   -> rookMoves
      Bishop -> bishopMoves
      Queen  -> queenMoves

moves :: Board -> (Int, Int) -> [(Int, Int)]
moves board (x, y) = filter noCheck $ rawMoves board (x, y)
  where
    noCheck (eX, eY) = not . check c $ move (x, y) (eX, eY) board
    c = colour $ board ! (y, x)

eventIfOngoing :: Event -> World -> World
eventIfOngoing event world =
  if world.gameState /= Ongoing && isKeyPress
     then world
     else execState (events event) world
  where
    isKeyPress = case event of
      (EventKey _ _ _ _) -> True
      _                  -> False

checkWin :: World -> World
checkWin world =
  if noMoves
    then world { gameState = if check colour b then colourToWin $ other colour else Draw }
    else world
  where
    colour = world.turn
    b = world.board
    noMoves = null . fold $ moves b <$> side colour b

events :: Event -> State World ()
events (EventKey (MouseButton LeftButton) keyState _ (toSquare -> Just x, toSquare -> Just y)) = do
  World board selected _ turn _ _ <- get
  let click = (x, y)
  case (keyState, selected) of
    (Down, Nothing) -> selectIfValid board turn click
    (Down, Just selection) -> do
      moved <- moveIfValid board click selection
      when (not moved && click /= selection) $ selectIfValid board turn click
    (Up, Just selection) | click /= selection -> void $ moveIfValid board click selection
    _ -> pure ()
  where
    selectIfValid board turn click = case board ! swap click of
      Piece _ colour _ | colour == turn -> #selected ?= click
      _ -> pure ()
    moveIfValid board click selection = do
      #selected .= Nothing
      let good = elem click $ moves board selection
      when (good) do
        #board    %= move selection click
        #lastMove ?= click
        #turn     %= other
        modify checkWin
      pure good
events (EventResize dims) = #dims .= dims
events _ = pure ()


home :: [Chessman]
home = [Rook, Horsey, Bishop, King, Queen, Bishop, Horsey, Rook]

start :: Board
start = lazyGridMap (rectOctGrid 8 8) $
  fold [ Piece 0 Black <$> home
       , replicate 8 $ Piece 0 Black Pawn
       , replicate 32 Empty
       , replicate 8 $ Piece 0 White Pawn
       , Piece 0 White <$> home
       ]

imgNames :: [String]
imgNames = liftA2 (\p c -> [p, c]) "kqrnbp" "dl"

pieceOrder :: [Piece]
pieceOrder = liftA2 (&) [King, Queen, Rook, Horsey, Bishop, Pawn] [Piece 0 Black, Piece 0 White]


main :: IO ()
main = do
  path <- getCurrentDirectory
  imgs <- for imgNames $ \name -> loadBMP $ fold [path, "/imgs/", name, ".bmp"]
  Right bmp <- readBMP $ path <> "/imgs/kd.bmp"

  let factor     = q/4 / (fromIntegral . fst $ bmpDimensions bmp) -- it's a square.
      scaledImgs = Scale factor factor <$> imgs
      w          = round $ q*2
      dims       = (w, w)
      window     = InWindow "caos9df8gukdsasd9hfjlksas!!!" dims (10, 10)
      world = World { board     = start
                    , selected  = Nothing
                    , lastMove  = Nothing
                    , turn      = White
                    , dims      = dims
                    , gameState = Ongoing
                    }

  play window white 0 world (convert . M.fromList $ zip pieceOrder scaledImgs) eventIfOngoing $ flip const
