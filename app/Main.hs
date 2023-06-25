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
import Graphics.Gloss.Interface.IO.Interact
import Math.Geometry.GridMap hiding (filter)
import Data.Set hiding (fold, insert, filter, null, take, drop, elems)


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 7 .:

twoArray :: a -> a -> [a]
twoArray x y = [x, y]


data Colour = White | Black deriving (Eq, Ord)
other :: Colour -> Colour
other Black = White
other White = Black

type Board = LGridMap RectOctGrid Piece
data GameState = Ongoing | Promoting (Int, Int) | WhiteWin | BlackWin | Draw deriving Eq
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
  , positions :: M.Map String Int -- number of times positions have occured
  , fiftyMove :: Int              -- number of turns since both sides has captured/moved a pawn (so 100 for 50 moves)
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

promotingBL :: (Int, Int) -> (Float, Float)
promotingBL (x, y) = (blX, blY)
  where
    blX = fromIntegral x * q/4 - q + q/8 - q/2
    blY = fromIntegral (y + (signum $ y - 2)) * q/4 - q

promotingPieces :: [Chessman]
promotingPieces = [Queen, Horsey, Bishop, Rook]

drawPromoting :: M.Map Piece Picture -> Colour -> (Int, Int) -> Picture
drawPromoting imgs colour (x, y) = Pictures $ zipWith (\icon (sX, sY) -> Translate sX sY icon) icons squareCoords
  where
    icons        = (\p -> Pictures $ outline <> [Translate (q/8) (q/8) . (imgs M.!) $ Piece 0 colour p]) <$> promotingPieces
    outline      = liftA2 twoArray (Color (greyN 0.8) . Polygon) Line $ square 0 0 (q/4)
    squareCoords = take 4 . zip [blX, blX + q/4 ..] $ repeat blY
    (blX, blY)   = promotingBL (x, y)

-- todo: make this change with window sizes
convert :: M.Map Piece Picture -> World -> Picture
convert imgs world = Pictures $
  ([background world.dims bColour, drawBoard imgs world, border] <>
   [ case world.gameState of
       Ongoing     -> Blank
       Promoting c -> drawPromoting imgs world.turn c
       WhiteWin    -> say black "White wins!"
       BlackWin    -> say white "Black wins!"
       Draw        -> say black "It's a draw!"
   ])
  where
    say c s = Color c . Translate -400 350 $ Text s
    turnColour = if world.turn == White then white else black
    bColour = case world.gameState of
      Ongoing     -> turnColour
      Promoting _ -> turnColour
      WhiteWin    -> white
      BlackWin    -> black
      Draw        -> greyN 0.8


-- rook -> king, rook
castleCoords :: (Int, Int) -> ((Int, Int), (Int, Int))
castleCoords (rX, rY) = ((kEndX, rY), (rEndX, rY))
  where
    rEndX = kEndX + signum (kEndX - rX)
    kEndX = div (3 + rX) 2


-- just swaps the 2 pieces
rawMove :: (Int, Int) -> (Int, Int) -> Board -> Board
rawMove s e board = insert (swap e) (piece { nMoves = piece.nMoves + 1 }) $ insert (swap s) Empty board
  where
    piece = board ! swap s

-- returns square to highlight and board
move :: (Int, Int) -> (Int, Int) -> Board -> ((Int, Int), Board)
move s@(sX, sY) e@(eX, eY) board =
  case man of
    King ->
      if (abs $ sX - eX) > 1
         then castle s (if eX < sX then 0 else 7, eY)
         else normalMove
    Rook ->
      if e == kingIdx colour board
         then castle e s
         else normalMove
    Pawn ->
      if (abs $ sX - eX) == 1 && board ! (eY, eX) == Empty
         then insert (sY, eX) Empty <$> normalMove -- en passant
         else normalMove
    _ -> normalMove
  where
    castle king rook =
      let (castledK, castledR) = castleCoords rook in
      (castledK, rawMove rook castledR $ rawMove king castledK board)
    normalMove = (e, rawMove s e board)
    Piece { man, colour } = board ! swap s

isCol :: Colour -> Piece -> Bool
isCol c = \case
  Piece { colour } | colour == c -> True
  _ -> False

notCol :: Board -> Colour -> (Int, Int) -> Bool
notCol board colour (x, y) = not . isCol colour $ board ! (y, x)

type GetMoves = (Int, Int) -> Colour -> Board -> [(Int, Int)]

pawnMoves :: (Int, Int) -> (Int, Int) -> Colour -> Board -> [(Int, Int)]
pawnMoves lastMove (x, y) colour board = enPassant diagL <> enPassant diagR <> normalMoves
  where
    enPassant diag = if otherPawn == lastMove && moved2Pawn otherPawn then [diag] else []
      where otherPawn = (- dir) <$> diag
    moved2Pawn coords = case lookup (swap coords) board of
      Just Piece { man = Pawn, nMoves = 1, colour = c } | c == other colour -> True
      _                                                                     -> False
    normalMoves = [p | (p, f) <- cases, maybe False f $ lookup (swap p) board]
    cases = [ ((x,   y+dir),   (Empty ==))
            , ((x,   y+dir*2), (nMoves == 0 &&) . (Empty ==))
            , (diagL,          isCol $ other colour)
            , (diagR,          isCol $ other colour)
            ]
    diagL = (x-1, y+dir)
    diagR = (x+1, y+dir)
    dir = if colour == Black then 1 else -1
    Piece { nMoves } = board ! (y, x)

canCastle :: (Int, Int) -> Colour -> Board -> Bool
canCastle (rX, rY) colour board =
  cornerRook && king.nMoves == 0 && rook.nMoves == 0 && noPieces && safe
  where
    safe = all (`notMember` bads) $ drop 2 lineCoords
    bads = threatening board $ other (board ! (rY, rX)).colour
    noPieces = all ((== Empty) . (board !) . swap) . init $ tail lineCoords
    lineCoords = zip [rX, rX + signum (kX - rX) .. kX] $ repeat rY
    cornerRook = case rook of
      Piece { nMoves = 0, man = Rook } -> True
      _                                -> False
    rook = board ! (rY, rX)
    king = board ! (kY, kX)
    (kX, kY) = kingIdx colour board


kingMoves :: Bool -> (Int, Int) -> Colour -> Board -> [(Int, Int)]
kingMoves noCastle (x, y) colour board = castleKingside <> castleQueenside <> normalMoves
  where
    normalMoves     = filter (notCol board colour) $ neighbours board (x, y)
    castleKingside  = castleMoves (0, y)  [(0, y), (2, y)]
    castleQueenside = castleMoves (7, y) [(7, y), (5, y)]
    castleMoves rook clicks = if not noCastle && canCastle rook colour board then clicks else []

horseyMoves :: GetMoves
horseyMoves (x, y) colour board = filter (\c -> contains board c && notCol board colour c) $
  [(mX+x, mY+y) | mX <- [-2..2], mY <- [-2..2], (abs $ mX*mY) == 2]

lineMoves :: (Int, Int) -> Colour -> Board -> (Int, Int) -> [(Int, Int)]
lineMoves (x, y) colour board (cX, cY) =
  take (min noSame tillTake) toEdge
  where
    noSame   = length $ takeWhile (notCol board colour) toEdge
    tillTake = 1 + (length $ takeWhile (notCol board (other colour)) toEdge)
    toEdge   = takeWhile (contains board) . tail $ zip [x, x+cX ..] [y, y+cY ..]

linesMoves :: [(Int, Int)] -> (Int, Int) -> Colour -> Board -> [(Int, Int)]
linesMoves dirs coords colour board = fold $ lineMoves coords colour board <$> dirs

rookMoves :: Bool -> (Int, Int) -> Colour -> Board -> [(Int, Int)]
rookMoves noCastle coords colour board = castle <> linesMoves [(1, 0), (-1, 0), (0, 1), (0, -1)] coords colour board
  where
    castle = if not noCastle && canCastle coords colour board then [kingIdx colour board] else []

bishopMoves :: GetMoves
bishopMoves = linesMoves [(1, 1), (-1, 1), (-1, -1), (1, -1)]

queenMoves :: GetMoves
queenMoves = linesMoves . filter (/= (0, 0)) $ liftA2 (,) [-1..1] [-1..1]

side :: Colour -> Board -> [(Int, Int)]
side colour board = [coords | coords <- indices board, isCol colour $ board ! swap coords]

threatening :: Board -> Colour -> Set (Int, Int)
threatening board colour = unions $ fromList . rawMoves (8, 8) board True <$> side colour board

kingIdx :: Colour -> Board -> (Int, Int)
kingIdx colour board = fromJust . find isKing $ indices board
  where
    isKing coords = case (board ! swap coords) of
      Piece { colour = c, man = King } | c == colour -> True
      _ -> False


check :: Colour -> Board -> Bool
check colour board = member (kingIdx colour board) . threatening board $ other colour


-- moves without checking if they would reveal check
rawMoves :: (Int, Int) -> Board -> Bool -> (Int, Int) -> [(Int, Int)]
rawMoves lastMove board noCastle (x, y) =
  pieceMoves (x, y) colour board
  where
    Piece { colour, man } = board ! (y, x)
    pieceMoves = case man of
      Horsey -> horseyMoves
      King   -> kingMoves noCastle
      Pawn   -> pawnMoves lastMove
      Rook   -> rookMoves noCastle
      Bishop -> bishopMoves
      Queen  -> queenMoves

moves :: (Int, Int) -> Board -> (Int, Int) -> [(Int, Int)]
moves lastMove board (x, y) = filter noCheck $ rawMoves lastMove board False (x, y)
  where
    noCheck (eX, eY) = not . check c . snd $ move (x, y) (eX, eY) board
    c = colour $ board ! (y, x)

gameToString :: World -> String
gameToString world = toStringC world.turn <> (fold $ toString <$> elems world.board)
  where
    toString = \case
      Empty -> "e"
      Piece { colour = c, man = m } -> toStringC c <> toStringM m
    toStringC = \case
      Black -> "b"
      White -> "w"
    toStringM = \case
      Pawn   -> "p"
      Horsey -> "n"
      King   -> "k"
      Queen  -> "q"
      Bishop -> "b"
      Rook   -> "r"

checkWin :: State World ()
checkWin = do
  World { board, lastMove, turn, fiftyMove } <- get

  --checkmate/stalemate
  let noMoves = null . fold $ moves (fromJust lastMove) board <$> side turn board
  when noMoves $
    #gameState .= if check turn board then colourToWin $ other turn else Draw

  -- draw by repetition
  -- NB. i'm not checking if castling/en passant rights are the same because that's some pedantic nerd stuff
  -- (and i'm too lazy to)
  pos <- gets gameToString
  #positions %= M.insertWith (+) pos 1
  poses <- use #positions
  when (poses M.! pos == 3) $ #gameState .= Draw


  -- 50 move rule
  when (fiftyMove == 100) $ #gameState .= Draw

toPromotingSquare :: (Int, Int) -> (Float, Float) -> Maybe Int
toPromotingSquare (pX, pY) (x, y) =
  if y < blY || blY + q/4 < y || notElem xSquare [0..3]
    then Nothing
    else Just xSquare
  where
    xSquare = floor $ (x - blX) / (q/4)
    (blX, blY) = promotingBL (pX, pY)

eventWhilePromoting :: World -> Event -> World
eventWhilePromoting world = \case
  (EventKey (MouseButton LeftButton) Down _ (toPromotingSquare (x, y) -> Just choice))
    -> world { board     = insert (y, x) (Piece 0 turn (promotingPieces !! choice)) board
             , turn      = other turn
             , gameState = Ongoing
             }
  _ -> world
  where
    World { board, turn, gameState = Promoting (x, y) } = world

toSquare :: Float -> Maybe Int
toSquare s = if elem c [0..7] then Just c else Nothing
  where c = 4 + floor (s / (q / 4))

normalEvent :: Event -> State World ()
normalEvent (EventKey (MouseButton LeftButton) keyState _ (toSquare -> Just x, toSquare -> Just y)) = do
  World { board, selected, turn } <- get
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
      Piece { colour } | colour == turn -> #selected ?= click
      _ -> pure ()
    moveIfValid board click selection = do
      #selected .= Nothing
      lastMove <- use #lastMove
      let good = elem click $ moves (fromMaybe (8, 8) lastMove) board selection
      when good do
        let (highlight, newBoard) = move selection click board
        #board    .= newBoard
        #lastMove ?= highlight
        let pawn = isPawn $ newBoard ! (y, x)

        turn <- use #turn
        if not $ pawn || (isCol (other turn) $ board ! (y, x))
          then #fiftyMove += 1
          else #fiftyMove .= 0
        checkWin

        if elem y [0, 7] && pawn
          then #gameState .= Promoting click
          else #turn      %= other

      pure good
    isPawn = \case
      Piece { man = Pawn } -> True
      _                    -> False
normalEvent (EventKey (Char 'z') _ _ _) = do
  turn <- use #turn
  board <- use #board
  let notQK p = p.man /= King && p.man /= Queen
  let pieces = filter (notQK . (board !) . swap) $ side turn board
  when (not $ null pieces) $ #board %= insert (swap $ head pieces) (Piece 0 turn Queen)
normalEvent (EventResize dims) = #dims .= dims
normalEvent _ = pure ()

events :: Event -> World -> World
events event world =
  case world.gameState of
    Promoting _                    -> eventWhilePromoting world event
    x | x /= Ongoing && not resize -> world
    _                              -> execState (normalEvent event) world
  where
    resize = case event of
      (EventResize _) -> True
      _                     -> False


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
imgNames = liftA2 twoArray "kqrnbp" "dl"

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
      world      = World { board     = start
                         , selected  = Nothing
                         , lastMove  = Nothing
                         , turn      = White
                         , dims      = dims
                         , gameState = Ongoing
                         , positions = M.fromList [(gameToString world, 1)]
                         , fiftyMove = 0
                         }

  play window white 0 world (convert . M.fromList $ zip pieceOrder scaledImgs) events $ flip const
