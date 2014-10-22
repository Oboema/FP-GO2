module Graphs where
{- 
        ( Label
        , Weight
        , Node
        , Edge
        , Graph(..)
        , ColorG(..)
        , Thickness(..)
        , Directed(..)
        , Weighted(..)
        , GraphOutput(..)
        , GraphInput(..)
        , Pos
        , MouseButton(..)
        , KeyboardButton
        , onNode
        , preEventloop
        ) 
-} 

import Prelude
import EventLoop        
import EventLoop.Input as EI
import EventLoop.Output


type Vector = (Float, Float)
type Weight = Int -- weight of a weighted graph, kept for compatibility. We use Mass for masses
----- Graph -----
-- type Vector     = (FLoat, Float)
type Force      = (Vector, Float)
type Elasticity = Float -- yet to be defined. Use 1 for now.

type Label    = String
type Mass     = Float
type EdgeForce= Float

data Tolerance  = Tolerance 
                { pressForce :: Float
                , pullForce  :: Float
                }   deriving (Eq, Show)


type MetaEdge = (EdgeForce, Tolerance, Elasticity, Mass)

type Node = (Label, Pos, Force)
    --deriving Show
type Edge = (Label, Label, MetaEdge )
    --deriving Show

data Graph = Graph
            { nodes    :: [Node]
            , edges    :: [Edge]
            , directed :: Directed
            , weighted :: Weighted
            } deriving (Eq, Show)


----- Graph Graphical -----            
            
data ColorG = Red
            | Blue
            | Green
            | Purple
            | Grey
            | Yellow
            | Orange
            | Black
            | White
            deriving (Eq, Show)

data Thickness = Thin
               | Thick
               deriving (Eq, Show)

data Directed  = Directed
               | Undirected
                deriving (Eq, Show)
               
data Weighted  = Weighted
               | Unweighted
               deriving (Eq, Show)

-- | The output expected in a graph graphical program               
data GraphOutput = NodeG Label Pos ColorG
                 | LineG Node Node ColorG Thickness Directed 
                 | WeightedLineG Node Node Weight ColorG Thickness Directed
                 | Instructions [String]
                 | RemoveNodeG Label
                 | RemoveEdgeG Label Label
                 | ClearAllG
                 deriving (Eq, Show)

nodeRadius   = 20 :: Float                 
textSize     = 16 :: Float
textFont     = "Courier"
xArrowSize   = 6 :: Float
yArrowSize   = 6 :: Float
weightHeight = 10 :: Float

topInstructions = 440
dimCanvas = (840,840)
canvasWidth = fst dimCanvas
canvasHeight = snd dimCanvas
                      

-- | The input expected in a graph graphical program
data GraphInput = MouseUp MouseButton Pos
                | MouseDown MouseButton Pos
                | MouseClick MouseButton Pos
                | KeyPress KeyboardButton
                | Start
               



-- | Checkes to see if there is a node on a certain position                
onNode :: [Node] -> Pos -> Maybe Node
onNode [] _ = Nothing
onNode (n@(_, (nx, ny), _):ns) (x,y) | difference <= nodeRadius = Just n
                                     | otherwise                = onNode ns (x,y)
                                    where
                                        dx         = nx - x
                                        dy         = ny - y
                                        difference = sqrt (dx^2 + dy^2)
                 
-- | Starting point for the library. Call this function to start the eventloop with the given handler.
-- Takes 'FPPrac.Graph.Input' and returns 'FPPrac.Graph.Output' instead of the standardized 'EventLoop.Input'
-- and 'EventLoop.Output'.
preEventloop :: (a -> GraphInput -> ([GraphOutput], a)) -> a -> IO ()
preEventloop handler beginState = start handler' beginState
                                where
                                    handler' = changeTypes handler

-- | Changes the eventhandler to use the 'Graphs.GraphInput' instead of the 'EventLoop.Input.InputEvent'
-- Also catches the 'EventLoop.Input.InSysMessage.Setup' message and gives the correct dimensions.
changeTypes :: (a -> GraphInput -> ([GraphOutput], a)) -> a -> EI.InputEvent -> ([OutputEvent], a)
changeTypes _       state (EI.InSysMessage Setup) = ([OutSysMessage [CanvasSetup dimCanvas]],state)
changeTypes handler state inputE = (out, state')
                                where
                                    inputE'            = inputEventToGraphIn inputE
                                    (graphOut, state') = handler state inputE' 
                                    out                = map (\a -> OutGraphical a) $ concat $ map graphOutputToGraphical graphOut
                                    
-- | Abstracts the standardized 'EventLoop.Input.InputEvent' to 'Graphs.GraphInput'    
inputEventToGraphIn :: EI.InputEvent -> GraphInput
inputEventToGraphIn (EI.InKeyboard k) = keyboardToGraphIn k
inputEventToGraphIn (EI.InMouse m )   = mouseToGraphIn m
inputEventToGraphIn (EI.InSysMessage Background) = Start

-- | Abstracts the standardized 'EventLoop.Input.Mouse' to 'Graphs.GraphInput'
mouseToGraphIn :: EI.Mouse -> GraphInput
mouseToGraphIn (EI.MouseClick mb p _) = Graphs.MouseClick mb p
mouseToGraphIn (EI.MouseUp    mb p _) = Graphs.MouseUp mb p
mouseToGraphIn (EI.MouseDown  mb p _) = Graphs.MouseDown mb p

-- | Abstracts the standardized 'EventLoop.Input.Keyboard' to 'Graphs.GraphInput'
keyboardToGraphIn :: EI.Keyboard -> GraphInput
keyboardToGraphIn (EI.KeyPress k) = Graphs.KeyPress k    

{-|
Converts the graphical graph output to standardized 'Eventloop.Output.Graphical' output
-}
graphOutputToGraphical :: GraphOutput -> [Graphical]
graphOutputToGraphical (NodeG l pos colG) = [Draw (Container [nodeG, textG]) l]
                                        where
                                            col   = colorGToColor colG
                                            nodeG = GObject l (Arc black 1 col pos nodeRadius 0 360) []
                                            textG = GObject l (Text white 1 white pos textSize textFont l True) []

graphOutputToGraphical (LineG (l1, pos1, _) (l2, pos2, _) colG thick direct) | direct == Directed   = [Draw (Container [line, arrow1, arrow2]) name]
                                                                             | direct == Undirected = [Draw line name]
                                                                            where
                                                                                name   = lineName l1 l2
                                                                                col    = colorGToColor colG
                                                                                thick' = thicknessToFloat thick
                                                                                line   = GObject name (Line col thick' [lineStart, lineEnd]) []
                                                                                arrow1 = GObject name (Line col thick' [arrowStart, arrow1End]) []
                                                                                arrow2 = GObject name (Line col thick' [arrowStart, arrow2End]) []
                                                                                --Vector stuff
                                                                                lineVector         = vectorize pos1 pos2
                                                                                lineVector'        = vectorize pos2 pos1
                                                                                lineStart          = posOnVector nodeRadius lineVector pos1
                                                                                lineEnd            = posOnVector nodeRadius lineVector' pos2
                                                                                arrowPerpStart     = posOnVector xArrowSize lineVector' lineEnd  
                                                                                upPerpLineVector   = upPerpendicularTo pos1 pos2
                                                                                downPerpLineVector = downPerpendicularTo pos1 pos2
                                                                                arrowStart         = lineEnd
                                                                                arrow1End          = posOnVector yArrowSize upPerpLineVector arrowPerpStart  
                                                                                arrow2End          = posOnVector yArrowSize downPerpLineVector arrowPerpStart 

graphOutputToGraphical (WeightedLineG n1@(l1, pos1, _) n2@(l2, pos2, _) w colG thick direct) = lineGraphical ++ [Draw text name]
                                                                where
                                                                    name          = lineName l1 l2
                                                                    col           = colorGToColor colG
                                                                    lineGraphical = graphOutputToGraphical (LineG n1 n2 colG thick direct)
                                                                    text          = GObject name (Text col 1 col textPos textSize textFont (show w) True) []
                                                                    --Vector stuff
                                                                    lineVector'       = vectorize pos2 pos1
                                                                    halfSize          = vectorSize lineVector' / 2
                                                                    upPerpLineVector  = upPerpendicularTo pos2 pos1
                                                                    textPerpStart     = posOnVector halfSize lineVector' pos2
                                                                    textPos           = posOnVector weightHeight upPerpLineVector textPerpStart

graphOutputToGraphical (Instructions is) = [RemoveGroup "instructions", Draw isG' "instructions"]
                                        where
                                            lineG       = GObject "instructions" (Line black lineHeight [(0,topInstructions), (canvasWidth, topInstructions)]) []
                                            defaultText = (\str pos -> GObject "instructions" (Text black 1 black pos textSize textFont str False) [])
                                            lineHeight  = 2
                                            textMargin  = 2
                                            positions   = iterate ((+) (textSize + textMargin)) (topInstructions + lineHeight)
                                            isWithPos   = zip is positions
                                            isG         = map (\(str, top) -> defaultText str (0, top)) isWithPos
                                            isG'        = Container (lineG:isG)

graphOutputToGraphical (RemoveNodeG l)     = [RemoveGroup l]
graphOutputToGraphical (RemoveEdgeG l1 l2) = [RemoveGroup (lineName l1 l2)]
graphOutputToGraphical (ClearAllG)         = [ClearAll]                                        
           
-- | Returns a standardized naming scheme for lines in the graph           
lineName :: String -> String -> String
lineName l1 l2 = "line."++l1++"."++l2
           
-- | Translates the thickness to a float           
thicknessToFloat :: Thickness -> Float
thicknessToFloat Thick = 2.0
thicknessToFloat Thin  = 1.0

-- | Translates color datatype to RGB codes
colorGToColor :: ColorG -> Color
colorGToColor Red    = (255, 0, 0)
colorGToColor Blue   = (0, 0, 255)
colorGToColor Green  = (0, 255, 0)
colorGToColor Purple = (255, 0, 255)
colorGToColor Grey   = (125, 125, 125)
colorGToColor Yellow = (255, 255, 0)
colorGToColor Orange = (255, 125, 0)
colorGToColor Black  = (0, 0, 0)
colorGToColor White  = (255, 255, 255)

black = colorGToColor Black                                    
white = colorGToColor White


-- | Returns the point when making a step f long from the point start in the direction of the vector. The length between start pos and result pos is always f.
posOnVector :: Float -> Vector -> Pos -> Pos
posOnVector f (xv, yv) (xStart, yStart) = (x, y)
                                        where
                                            x        = xStart + fraction * xv
                                            y        = yStart + fraction * yv
                                            fraction = f / size
                                            size     = vectorSize (xv, yv)

-- | Vector from p1 to p2 
vectorize :: Pos -> Pos -> Vector
vectorize (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)


-- | Returns the vector perpendicular on the given vector between the 2 points. Always has positive y and vector length 1; y is inverted in canvas
downPerpendicularTo :: Pos -> Pos -> Vector
downPerpendicularTo (x1, y1) (x2, y2) | y2 > y1   = ((-1) * sign * (abs yv) / size, (abs xv) / size)
                                      | otherwise = (       sign * (abs yv) / size, (abs xv) / size)
                                      where
                                          (xv, yv) = vectorize (x1, y1) (x2, y2)
                                          size     = vectorSize (xv, yv)
                                          sign     = case xv of
                                                        0 -> (-1)
                                                        _ -> xv / (abs xv)
                                            
                                            
-- | Returns the vector perpendicular on the given vector between the 2 points. Always has negative y and vector length 1; y is inverted in canvas
upPerpendicularTo :: Pos -> Pos -> Vector
upPerpendicularTo p1 p2 = ((-1) * xp, (-1) * yp)
                        where
                            (xp, yp) = downPerpendicularTo p1 p2
                          
-- | Returns the size of the vector                          
vectorSize :: Vector -> Float
vectorSize (x, y) = sqrt (x^2 + y^2)    
