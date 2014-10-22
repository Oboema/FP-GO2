module Main where

import Prelude
import System.Environment
import Graphs
import Debug.Trace

-- MetaEdge = (EdgeForce, Tolerance, Elasticity, Weight)
testTol 	= Tolerance { pressForce=1, pullForce=5 }
testElas 	= 1 :: Float
testWeight  = 0 :: Float
testEdgeF  	= 0 :: Float 

testMetaEdge = (testEdgeF, testTol, testElas, testWeight)

-- mass to gravitational force function
fz :: Float -> Force 
fz m = ((0,-1), m)

--Graph = Graph { nodes=[(Label, Float, Float)], edges=[(Label, Label, MetaEdge)]}
gr = Graph { nodes=[("k1", (33,44), (fz 0)  ), ("k3", (55, 21), (fz 0) ) ], edges=[("k1", "k3", testMetaEdge)]}


printGraph  :: Graph -> IO () --String
printGraph graph 	= 	putStr $  
						"\nNodes: " ++ (concat (map ("\n\t"++) (map show (nodes graph)) )  ) ++ 
						"\nEdges: " ++ (concat (map ("\n\t"++) (map show (edges graph)) )  )
--}

getNodes :: [String] -> ([Node],[String])
getNodes (('e':firstEdge):edges)	= ([], edges)
getNodes (nodeLine:nodeLines)		= (nodes',rest)
	where 
		[lab, xStr, yStr]	= words nodeLine
		[x, y]				= map read [xStr, yStr] :: [Float]
		node 				= (lab, (x,y), fz 0)
		(remNodes, rest)  	= getNodes nodeLines 
		nodes'				= node:remNodes

getEdges :: [String] -> ([Edge], [String])
getEdges []						= ([], []) 
getEdges (edgeLine:edgeLines) 	= (edges', rest)
	where
		[_, l1, l2] 		= words edgeLine
		edge 				= (l1, l2, testMetaEdge)
		(remEdges, rest)	= getEdges edgeLines
		edges'				= edge:remEdges

readGraph :: [String] -> Graph
readGraph glines 	= graph
	where
		(nodes', rest)	= getNodes glines
		(edges', rest2) = getEdges rest--([("a", "b", testMetaEdge)],"") --edgesFromFile rest
{-
		nodesFromFile ('e':edgs)	= []
		nodesFromFile line:s 		= (node:(nodesFromFile s), rest)

			where
				node 	= (nlab, pos, f)
				(a,b,c) =
-}
		graph = Graph 	{ nodes = nodes'
						, edges = edges'}

-- putWeight

main = do
    things <- getArgs
    gFile  <- readFile $ head things
    let graph = readGraph (lines gFile)
    printGraph graph

    return ()
