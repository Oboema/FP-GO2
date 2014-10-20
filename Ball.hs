import Prelude
import EventLoop
import EventLoop.Input
import EventLoop.Output

type Vector = (Float, Float)

type Speed = (Float, Vector)

type Position = (Float, Float)

data State = Store { speed :: Speed
                   , position :: Position
                   }
ballName = "Ball"
rad = 50 :: Float           
                   
beginState = Store beginSpeed beginPos
beginSpeed = (8, (0.75, 0.25))
beginPos   = (0.5 * widthCanvas, 0.5 * heightCanvas)

widthCanvas = 360 :: Float
heightCanvas = 450 :: Float

main = start process beginState

process :: State -> InputEvent -> ([OutputEvent], State)
process s (InSysMessage Setup) = ([OutSysMessage [(CanvasSetup (widthCanvas,heightCanvas)), (Timer (On 17))]], s)

process s (InSysMessage Background) = ([drawBall beginPos], s)

process (Store speed pos) (InSysMessage Time) = ([moveBall p], (Store s p))
                                            where
                                                (s, p) = newSpeedPosition speed pos

process s _ = ([], s)
                                                
drawBall :: Position -> OutputEvent
drawBall pos = OutGraphical (Draw ballG "")
                  where
                    ballG = GObject ballName ball []
                    ball  = Arc (0,0,0) 0 (255, 0, 0) pos rad 0 360

moveBall :: Position -> OutputEvent
moveBall pos = OutGraphical (MoveElement ballName pos False)


newSpeedPosition :: Speed -> Position -> (Speed, Position)
newSpeedPosition (s, (vX, vY)) (x, y) = (speed', pos')
                                         where
                                             speed' = newSpeed (s, (vX, vY)) pos'
                                             pos' = (x', y')
                                             x' = x + s * vX
                                             y' = y + s * vY

newSpeed :: Speed -> Position -> Speed
newSpeed speed pos = bounce (didBounce pos) speed
                                
didBounce :: Position -> Bounce
didBounce (x, y) | top && left     = TL
                 | top && right    = TR
                 | bottom && left  = BL
                 | bottom && right = BR
                 | top       = T
                 | left      = L
                 | right     = R
                 | bottom    = B
                 | otherwise = No
                     where
                         top    = inContactBorder 0 y
                         bottom = inContactBorder heightCanvas y
                         left   = inContactBorder 0 x
                         right  = inContactBorder widthCanvas x
                         
inContactBorder :: Float -> Float -> Bool
inContactBorder x1 x2 = rad > (abs (x1 - x2))
                             

data Bounce = L | R | T | B | TL | BL | TR | BR | No

bounce :: Bounce -> Speed -> Speed
bounce L  (v, (x, y)) = (v, (-x, y))
bounce R  (v, (x, y)) = (v, (-x, y))
bounce T  (v, (x, y)) = (v, (x, -y))
bounce B  (v, (x, y)) = (v, (x, -y))
bounce TL (v, (x, y)) = (v, (-x, -y))
bounce BL (v, (x, y)) = (v, (-x, -y))
bounce TR (v, (x, y)) = (v, (-x, -y))
bounce BR (v, (x, y)) = (v, (-x, -y))
bounce No speed = speed