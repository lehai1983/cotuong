import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent (Coord)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Tree
import System.Random

-- Types

data ETree n e = ENode {
    etRoot :: n,
    etChildren :: [(e, ETree n e)]
    }

data Side = Red | Blue deriving Eq

data PieceKind = Pawn | King deriving Eq

data Piece = Piece {
    pieceSide :: Side,
    pieceKind :: PieceKind
    }

type Pieces = Map Coord Piece

data Board = Board {
    bsPieces :: Pieces,
    bsSide :: Side
    }

data Action = Slide Coord | Capture [(Coord, Coord)]

type Move = (Coord, Action)

data HeldPiece = HeldPiece {
    hpCoordOriginal :: Coord, -- piece's original position on the board
    hpMouseOffset :: Point -- mouse's offset from piece when it was picked up
    }

type GameTree = ETree Board Move

data Game = Game {
    gmTree :: GameTree,
    gmMousePos :: Point,
    gmHeldPiece :: Maybe HeldPiece,
    gmDebugDropActive :: Bool,
    gmRandomSource :: StdGen
    }

type Score = Int

-- Values

boardSize :: Int
boardSize = 4

searchDepth :: Int
searchDepth = 3

gameBoard :: Game -> Board
gameBoard = etRoot . gmTree

gameNextTrees :: Game -> [GameTree]
gameNextTrees = (map snd) . etChildren . gmTree

hexRadius :: Float
hexRadius = 48

hexWidth :: Float
hexWidth = (sqrt 3) / 2

main :: IO ()
main = do
    r <- getStdGen
    play
        displayMode
        backgroundColor
        framesPerSecond
        (newGame r)
        drawGame
        handleInputEvent
        stepTime
    where displayMode = InWindow "HexDame" (sizeX, sizeY) (5, 5)
          backgroundColor = makeColorI 170 180 145 255
          framesPerSecond = 100
          sizeX = 768
          sizeY = 700

stepTime :: Float -> Game -> Game
stepTime _ = id

drawGame :: Game -> Picture
drawGame g = Scale hexRadius hexRadius $ Pictures [
    drawBackground,
    drawPieces g,
    drawMouseHighlight g,
    drawHeldPiece g ]

drawMouseHighlight :: Game -> Picture
drawMouseHighlight g = Pictures $ map (drawMove g) movesForCoord
    where
      moves = (etChildren . gmTree) g
      movesForCoord = filter ((== c) . fst . fst) moves
      c = case gmHeldPiece g of
        Just hp -> hpCoordOriginal hp
        Nothing -> gridFromScreen (gmMousePos g)

drawMove :: Game -> (Move, GameTree) -> Picture
drawMove g (m@(pos0, action), tree) = Pictures [movePic, scorePic]
  where
    movePic = case action of
      Slide pos1 -> highlightCoord pos1
      Capture cs -> Pictures $ highlightCaptures pos0 cs
    scorePic = translateCoord (finalDestination action) $ drawScore score
    score = (scoreTree tree) - baselineScore
    baselineScore = scoreBoard (gameBoard g)

drawScore :: Score -> Picture
drawScore score = Color c $ thickArc 0 (45 * (fromIntegral (abs score))) 0.7 0.1
  where
    c = if score > 0 then (light red) else (light blue)

highlightCaptures :: Coord -> [(Coord, Coord)] -> [Picture]
highlightCaptures pos0 captures = (highlightCoord finalDest) : (map (uncurry lineCoords) jumps)
  where
    jumps = zip (pos0 : dests) dests
    dests = map snd captures
    finalDest = last dests

lineCoords :: Coord -> Coord -> Picture
lineCoords pos0 pos1 = Color white $ Line [screenFromGrid pos0, screenFromGrid pos1]

highlightCoord :: Coord -> Picture
highlightCoord c = Color white $ translateCoord c hexHighlight

drawHeldPiece :: Game -> Picture
drawHeldPiece g = case (gmHeldPiece g) of
    Nothing -> Blank
    Just (HeldPiece coord offset) -> case pieceAt (bsPieces (gameBoard g)) coord of
      Nothing -> Blank
      Just piece -> drawPiece pos piece
      where
        pos = ((gmMousePos g) |+| offset) |*| (1 / hexRadius)

handleInputEvent :: Event -> Game -> Game
handleInputEvent e g = case e of
    (EventKey (Char 'd') Down _ pos) -> (onMouseMove pos g) { gmDebugDropActive = True }
    (EventKey (Char 'd') Up _ pos) -> (onMouseMove pos g) { gmDebugDropActive = False }
    (EventKey (Char 'k') Down _ pos) -> debugToggleKing (onMouseMove pos g)
    (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventKey (Char 'r') Down _ pos) -> doRandomMove (onMouseMove pos g)
    (EventKey (SpecialKey KeySpace) Down _ pos) -> doComputerMove (onMouseMove pos g)
    (EventKey (MouseButton LeftButton) Down _ pos) -> onMouseDown (onMouseMove pos g)
    (EventKey (MouseButton LeftButton) Up _ pos) -> onMouseUp (onMouseMove pos g)
    (EventMotion pos) -> onMouseMove pos g
    _ -> g

onMouseMove :: Point -> Game -> Game
onMouseMove p g = g { gmMousePos = p }

onMouseDown :: Game -> Game
onMouseDown g = case pieceAt (bsPieces (gameBoard g)) mouseGridPos of
    Nothing -> g
    Just _ -> g {
                gmHeldPiece = Just HeldPiece {
                                    hpCoordOriginal = mouseGridPos,
                                    hpMouseOffset = mouseScreenOffset }
                }
    where mouseScreenPos = gmMousePos g
          mouseGridPos = gridFromScreen mouseScreenPos
          mouseScreenOffset = ((screenFromGrid mouseGridPos) |*| hexRadius) - mouseScreenPos

onMouseUp :: Game -> Game
onMouseUp g = case (gmHeldPiece g) of
    Nothing -> g
    Just (HeldPiece c _) -> g''
      where
        g' = g { gmHeldPiece = Nothing }
        cNew = gridFromScreen (gmMousePos g)
        g'' = case legalMove g' c cNew of
          Nothing -> g'
          Just tree -> g' { gmTree = tree }

doRandomMove :: Game -> Game
doRandomMove g = case randomElement (gmRandomSource g) (gameNextTrees g) of
    Nothing -> g
    Just (tree, rnd) -> g { gmTree = tree, gmRandomSource = rnd }

doComputerMove :: Game -> Game
doComputerMove g = case randomElement (gmRandomSource g) bestMoves of
    Nothing -> g
    -- Just (tree, rnd) -> g { gmTree = tree, gmRandomSource = rnd }
    Just (tree, rnd) -> g { gmTree = gameTreeFrom (etRoot tree), gmRandomSource = rnd }
  where
    moves = gameNextTrees g
    scoredMoves = zip (map scoreTree moves) moves
    scoredMovesSorted = sortBy ((flip compare) `on` ((* scoreMult) . fst)) scoredMoves
    bestMoves = case scoredMovesSorted of
      [] -> []
      ((bestScore, _):_) -> map snd $ takeWhile ((== bestScore) . fst) scoredMovesSorted
    scoreMult = case (bsSide (gameBoard g)) of
      Red -> 1
      Blue -> -1

scoreTree :: GameTree -> Score
scoreTree = minimaxScoreTree searchDepth

minimaxScoreTree :: Int -> GameTree -> Score
minimaxScoreTree depth tree =
  if depth == 0 || null nextMoves
  then scoreBoard (etRoot tree)
  else minMax $ map (minimaxScoreTree (depth - 1)) nextMoves
  where
    nextMoves = map snd (etChildren tree)
    minMax = if (bsSide (etRoot tree)) == Red then maximum else minimum

scoreBoard :: Board -> Score
scoreBoard = sum . (map scorePiece) . Map.elems . bsPieces
  where
    scorePiece piece = case (pieceSide piece) of
      Red -> 1
      Blue -> -1

randomElement :: StdGen -> [a] -> Maybe (a, StdGen)
randomElement r xs = if (null xs) then Nothing else Just (xs!!i, r')
  where (i, r') = randomR (0, length xs - 1) r

debugToggleKing :: Game -> Game
debugToggleKing g = case pieceAt (bsPieces (gameBoard g)) coord of
    Nothing -> g
    Just _ -> g { gmTree = gameTreeFrom $ (gameBoard g) { bsPieces = Map.adjust toggleKind coord (bsPieces (gameBoard g))} }
    where
      coord = gridFromScreen (gmMousePos g)
      toggleKind piece = piece { pieceKind = oppositeKind (pieceKind piece) }
      oppositeKind King = Pawn
      oppositeKind Pawn = King

drawBackground :: Picture
drawBackground = Pictures [ Color (hexColor c) $ translateCoord c hex | c <- boardCoords ]

drawPieces :: Game -> Picture
drawPieces g = Pictures $ map drawPiece' pieces
  where
    drawPiece' (coord, piece) = drawPiece (screenFromGrid coord) piece
    pieces = case gmHeldPiece g of
      Nothing -> allPieces
      Just (HeldPiece coord _) -> filter ((/= coord) . fst) allPieces
    allPieces = Map.toList $ bsPieces $ gameBoard g

drawPiece :: Point -> Piece -> Picture
drawPiece pos (Piece side kind) = (uncurry Translate) pos $ Pictures $ case kind of
    Pawn -> [ Color color2 $ circleSolid 0.7, Color color1 $ circleSolid 0.6 ]
    King -> [ Color color2 $ rectangleSolid 1.2 1.2, Color color1 $ rectangleSolid 1 1 ]
    where color1 = sideColor side
          color2 = dark color1

newGame :: StdGen -> Game
newGame r = Game {
    gmTree = gameTreeFrom initialBoard,
    gmMousePos = (1000, 0),
    gmHeldPiece = Nothing,
    gmDebugDropActive = False,
    gmRandomSource = r
    }

initialBoard :: Board
initialBoard = Board {
    bsPieces = initialBoardPieces,
    bsSide = Blue
    }

initialBoardPieces :: Pieces
initialBoardPieces = Map.fromList (redPieces ++ bluePieces) where
    redPieces  = [ (( x,  y), Piece Red  Pawn) | (x, y) <- pieceCoords ]
    bluePieces = [ ((-x, -y), Piece Blue Pawn) | (x, y) <- pieceCoords ]
    pieceCoords = [ (x, y) | x <- [1..boardSize], y <- [1..boardSize] ]

boardCoords :: [Coord]
boardCoords = [ (i, j) |
    i <- [-boardSize..boardSize],
    j <- [-boardSize..boardSize],
    j - i <= boardSize,
    i - j <= boardSize ]

isOnBoard :: Coord -> Bool
isOnBoard (i, j) =
    i >= -boardSize &&
    j >= -boardSize &&
    i <= boardSize &&
    j <= boardSize &&
    j - i <= boardSize &&
    i - j <= boardSize

hexColor :: Coord -> Color
hexColor = cycleColors [ (85, 106, 47), (94, 117, 52), (77, 96, 43) ]

sideColor :: Side -> Color
sideColor side = case side of
    Red -> makeColorI 255 190 180 255
    Blue -> makeColorI 180 190 255 255

cycleColors :: [ (Int, Int, Int) ] -> Coord -> Color
cycleColors rgbs (i, j) = colors!!((i + j) `mod` (length colors))
    where colors = map (\(r, g, b) -> makeColorI r g b 255) rgbs

translateCoord :: Coord -> Picture -> Picture
translateCoord = (uncurry Translate) . screenFromGrid

screenFromGrid :: Coord -> Point
screenFromGrid (i, j) = (x, y)
    where x = fromIntegral (i + j) * hexWidth
          y = fromIntegral (i - j) * 1.5

gridFromScreen :: Point -> Coord
gridFromScreen (xScreen, yScreen) = (i, j)
    where xScaled = xScreen / (hexRadius * hexWidth)
          yScaled = yScreen / hexRadius
          yI = floor (yScaled + xScaled / 2)
          yJ = floor (yScaled - xScaled / 2)
          xIJ = floor xScaled
          i = floor ((fromIntegral (xIJ + yI + 2 :: Int)) / 3 :: Float)
          j = floor ((fromIntegral (xIJ - yJ + 1 :: Int)) / 3 :: Float)

hex :: Picture
hex = Polygon [ (0, -1), (x, -y), (x, y), (0, 1), (-x, y), (-x, -y) ]
    where x = hexWidth
          y = 0.5

hexHighlight :: Picture
hexHighlight = Circle 0.8

pieceAt :: Pieces -> Coord -> Maybe Piece
pieceAt board coord = Map.lookup coord board

isEnemy :: Board -> Coord -> Bool
isEnemy board coord = isOnBoard coord && case pieceAt (bsPieces board) coord of
    Nothing -> False
    Just piece -> (bsSide board) /= pieceSide piece

isEmpty :: Board -> Coord -> Bool
isEmpty board coord = isOnBoard coord && isNothing (pieceAt (bsPieces board) coord)

movesForPiece :: Board -> Coord -> Piece -> [Move]
movesForPiece board coord (Piece side kind) = map (\x -> (coord, x)) $
    if not (null captures)
      then map Capture (longest captures)
      else map Slide slides
    where
      board' = board { bsSide = side }
      captures = concatMap treePaths $ captureMoves capturePos board' coord
      slides = slidesFunc board' coord
      capturePos = case kind of
        Pawn -> pawnCapturePos
        King -> kingCapturePos
      slidesFunc = case kind of
        Pawn -> pawnSlides
        King -> kingSlides

gameTreeFrom :: Board -> GameTree
gameTreeFrom board = ENode board childStates
  where
    childStates = zip moves (map doMove moves)
    doMove = gameTreeFrom . (executeMove board)
    moves = movesForBoard board

executeMove :: Board -> Move -> Board
executeMove board (pos0, action) = case pieceAt (bsPieces board) pos0 of
    Nothing -> board
    Just p -> Board { bsPieces = bNew, bsSide = nextPlayer }
      where
        bNew = bsPieces board |> removePieceFromOldPos |> removeCapturedPieces |> insertPieceAtNewPos
        removePieceFromOldPos = Map.delete pos0
        removeCapturedPieces = case action of
          Slide _ -> id
          Capture c -> removeCaptures c
        insertPieceAtNewPos = Map.insert pos (promoteToKing pos p)
          where
            pos = finalDestination action
        nextPlayer = toggleSide (bsSide board)

promoteToKing :: Coord -> Piece -> Piece
promoteToKing coord piece = if (finalRow (pieceSide piece) coord)
    then piece { pieceKind = King }
    else piece

finalRow :: Side -> Coord -> Bool
finalRow side (i, j) = case side of
    Blue -> i >= boardSize || j >= boardSize
    Red -> i <= -boardSize || j <= -boardSize

removeCaptures :: [(Coord, Coord)] -> Pieces -> Pieces
removeCaptures captures board = foldl (flip Map.delete) board (map fst captures)

pawnSlides :: Board -> Coord -> [Coord]
pawnSlides board coord = stepMoves board coord (pawnDirs (bsSide board))

kingSlides :: Board -> Coord -> [Coord]
kingSlides board coord = concatMap (slideMoves board coord) allDirs

stepMoves :: Board -> Coord -> [Coord] -> [Coord]
stepMoves board coord possibleMoves = coordsNext
    where coordsNext = filter (isEmpty board) $ map (|+| coord) possibleMoves

slideMoves :: Board -> Coord -> Coord -> [Coord]
slideMoves board coord dir = let dest = coord |+| dir in
    if (isEmpty board dest) then (dest : (slideMoves board dest dir)) else []

captureMoves :: (Board -> Coord -> Coord -> Coord) -> Board -> Coord -> [Tree (Coord, Coord)]
captureMoves posFunc board = capturesFrom Set.empty
  where
    capturesFrom :: Set Coord -> Coord -> [Tree (Coord, Coord)]
    capturesFrom captured pos = mapMaybe (captures captured pos) allDirs

    captures :: Set Coord -> Coord -> Coord -> Maybe (Tree (Coord, Coord))
    captures captured pos0 dir = case validMove of
        True -> Just (Node (pos1, pos2) (capturesFrom (Set.insert pos1 captured) pos2))
        False -> Nothing
      where
        validMove =
          (isEnemy board pos1) &&
          (isEmpty board pos2) &&
          (Set.notMember pos1 captured)
        pos1 = posFunc board pos0 dir
        pos2 = pos1 |+| dir

pawnCapturePos :: Board -> Coord -> Coord -> Coord
pawnCapturePos _ pos0 dir = pos0 |+| dir

kingCapturePos :: Board -> Coord -> Coord -> Coord
kingCapturePos board pos0 dir = slide pos0
  where
    slide pos = let posNext = pos |+| dir in
      if (isEmpty board posNext) then slide posNext else posNext

treePaths :: Tree a -> [[a]]
treePaths (Node root nodes) = if null nodes then [[root]] else map (root:) (concatMap treePaths nodes)

longest :: [[a]] -> [[a]]
longest lists = filter ((== maxLength) . length) lists
    where
        maxLength = maximum (map length lists)

finalDestination :: Action -> Coord
finalDestination action = case action of
    Slide c -> c
    Capture cs -> snd (last cs)

legalMove :: Game -> Coord -> Coord -> Maybe GameTree
legalMove game pos0 pos1 = if (gmDebugDropActive game)
    then Just (gameTreeFrom (executeMove (gameBoard game) (pos0, Slide pos1)))
    else if null movesFiltered then Nothing else Just $ snd $ head movesFiltered
    where
      moves = etChildren (gmTree game)
      movesFrom = filter ((== pos0) . fst . fst) moves
      movesFiltered = filter ((== pos1) . finalDestination . snd . fst) movesFrom

movesForBoard :: Board -> [Move]
movesForBoard board = if not (null captureMoves) then longestCaptureMoves else unfilteredMoves
  where
    longestCaptureMoves = filter ((== longestMoveLength) . moveLength) captureMoves
    longestMoveLength = maximum (map moveLength captureMoves)
    captureMoves = filter moveIsCapture unfilteredMoves
    unfilteredMoves = concatMap (uncurry (movesForPiece board)) currentPieces
    currentPieces = filter ((== currentPlayer) . pieceSide . snd) $ Map.toList (bsPieces board)
    currentPlayer = bsSide board

moveIsCapture :: Move -> Bool
moveIsCapture (_, Slide _) = False
moveIsCapture (_, Capture _) = True

moveLength :: Move -> Int
moveLength (_, Slide _) = 0
moveLength (_, Capture c) = length c

pawnDirs :: Side -> [ Coord ]
pawnDirs side = case side of
    Red -> [(-1, -1), (-1, 0), (0, -1)]
    Blue -> [(1, 1), (1, 0), (0, 1)]

allDirs :: [ Coord ]
allDirs = [ (-1, -1), (-1, 0), (0, -1), (1, 0), (0, 1), (1, 1) ]

toggleSide :: Side -> Side
toggleSide Red = Blue
toggleSide Blue = Red

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(i0, j0) |+| (i1, j1) = (i0 + i1, j0 + j1)

(|*|) :: Num a => (a, a) -> a -> (a, a)
(x, y) |*| s = (x * s, y * s)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
