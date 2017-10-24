{-Chuong trinh co tuong-}
import AI
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
--import Graphics.Gloss.Juicy (loadJuicy)
import Data.Maybe
import System.Random

gridSize = 60
lineWidth = 2

data Game = Game 
	{
    theco :: Theco,
    mousePos :: Point,
	gmRandomSource :: StdGen
    } deriving (Show)

main :: IO ()
main = do
    r <- newStdGen
    play
        displayMode
        backgroundColor
        framesPerSecond
        (newGame r)
        drawGame
        handleInputEvent
        stepTime
    where displayMode = InWindow "CoTuong" (sizeX, sizeY) (5, 5)
          backgroundColor = makeColorI 255 255 255 255
          framesPerSecond = 100
          sizeX = 800
          sizeY = 600
		  
newGame :: StdGen -> Game
newGame r = Game { theco = (vanmoi T), mousePos = (500, 300), gmRandomSource = r}
		  
stepTime :: Float -> Game -> Game
stepTime _ = id

handleInputEvent :: Event -> Game -> Game
handleInputEvent (EventKey (Char 'n') Down _ _) g                 = newGame (mkStdGen 100)
handleInputEvent (EventKey (SpecialKey KeySpace) Down _ _) g      = doRandomMove g
handleInputEvent (EventKey (Char 'c') Down _ _) g                 = doComputerMove g
--handleInputEvent (EventKey (MouseButton LeftButton) Down _ pos) g =  
--handleInputEvent (EventKey (MouseButton LeftButton) Up _ pos) g   = 
handleInputEvent (EventMotion pos) g                              = g { mousePos = pos }
handleInputEvent _ g                                              = g

doRandomMove :: Game -> Game
doRandomMove g = g { theco = (dstheco (theco g)) !! (fst (randomR (1,luongGia (theco g)) (gmRandomSource g))) }

doComputerMove :: Game -> Game
doComputerMove g = g { theco = mintc (dstheco (theco g)) }
	
-- bieu dien Game bang hinh anh
drawGame :: Game -> Picture
drawGame g@(Game theco _ _) = Pictures [drawBanco, drawdsQutd theco]
	

drawBanco :: Picture
drawBanco = Color (makeColorI 35 35 35 255) $ Pictures
    [ Translate (-r*1) (5*r')  keDoc
	, Translate (-r*2) (5*r')  keDoc
	, Translate (-r*3) (5*r')  keDoc
	, Translate (-r*4) (5*r')  keDoc
	, Translate 0 (5*r')  keDoc
    , Translate (1*r) (5*r')     keDoc
	, Translate (2*r) (5*r')     keDoc
	, Translate (3*r) (5*r')     keDoc
	, Translate (4*r) (5*r')     keDoc
	
	, Translate (-r*1) (-r'*5)  keDoc
	, Translate (-r*2) (-r'*5)  keDoc
	, Translate (-r*3) (-r'*5)  keDoc
	, Translate (-r*4) (-r'*5)  keDoc
	, Translate 0 (-r'*5)  keDoc
    , Translate (1*r) (-r'*5)     keDoc
	, Translate (2*r) (-r'*5)     keDoc
	, Translate (3*r) (-r'*5)     keDoc
	, Translate (4*r) (-r'*5)     keDoc
	
	, Translate (-4*r + 5) (-r'*1 -15) haf
    , Translate 0 (-r'*1)  keNgang
	, Translate 0 (-r'*3)  keNgang
	, Translate 0 (-r'*5)  keNgang
	, Translate 0 (-r'*7)  keNgang
	, Translate 0 (-r'*9)  keNgang
    , Translate 0 (1*r')     keNgang
	, Translate 0 (3*r')     keNgang
	, Translate 0 (5*r')     keNgang
	, Translate 0 (7*r')     keNgang
	, Translate 0 (9*r')     keNgang
	, translateToado (H1,C5) (Rotate 45 (rectangleSolid lineWidth l))
	, translateToado (H1,C5) (Rotate (-45) (rectangleSolid lineWidth l))
	, translateToado (H8,C5) (Rotate 45 (rectangleSolid lineWidth l))
	, translateToado (H8,C5) (Rotate (-45) (rectangleSolid lineWidth l))
	]
    where r = gridSize ; r' = gridSize / 2; l = gridSize * (sqrt 8); 
		  keNgang = rectangleSolid (gridSize * 8) lineWidth ;
		  keDoc = rectangleSolid lineWidth (gridSize * 4) ;
		  haf = text "~~-~~"
		  
drawdsQutd :: Theco -> Picture
drawdsQutd tc = Pictures $ map drawQutd (dsQutd tc)

drawQutd :: Qutd -> Picture
drawQutd ((lq,ms),td) =
    Color (quanColor ms) $ translateToado td $ loaiQuanPicture lq
 
quanColor :: Maus -> Color
quanColor D = makeColorI 206 16 16 255
quanColor T = makeColorI 16 16 206 255

--translateToado :: Toado -> Picture -> Picture
translateToado :: (Enum h, Enum c) => (h, c) -> Picture -> Picture
translateToado (h, c) = Translate x y
    where x = (fromIntegral (fromEnum c - 4)) * gridSize
          y = (fromIntegral (2*(fromEnum h) - 9)) * (gridSize / 2)

loaiQuanPicture :: Loaiq -> Picture
loaiQuanPicture lq = case lq of
    Ts  -> Pictures [ ThickCircle 20 2, circleSolid 15]
    To  -> Pictures [ ThickCircle 20 2, Polygon [ (0, 15), (15, 0), (-15, 0) ] , Translate 0 (-5) $ circleSolid 10]
    Xe  -> Pictures [ ThickCircle 20 2, Translate 0 (5) $ rectangleSolid 30 20, Translate (-8) (-5) $ circleSolid 5, Translate 8 (-5) $ circleSolid 5 ]
    Ph  -> Pictures [ ThickCircle 20 2, rectangleSolid 10 30, Translate (-8) (-5) $ circleSolid 5, Translate 8 (-5) $ circleSolid 5 ]
    Tj  -> Pictures [ ThickCircle 20 2, Translate (-8) 0 $ circleSolid 10, Translate 5 5 $ circleSolid 8, Translate 10 0 $ rectangleSolid 5 20 ]
    Sy  -> Pictures [ ThickCircle 20 2, Rotate 45 $ rectangleSolid 20 20 ]
    Ma  -> Pictures [ ThickCircle 20 2, Rotate (-45) $ Translate 0 0 $ rectangleSolid 20 10, Translate 0 (-10) $ circleSolid 10, Translate 0 10 $ circleSolid 5 ]
