data Cell = Alive | Dead
	deriving (Show, Eq)

data World = World Int Int [Cell]
	deriving Show

initWorld :: Int -> Int -> World
initWorld w h = World w h [initCell (width * height) | width <- [1..w], height <- [1..h]]

initCell :: Int -> Cell
initCell n
	| mod n 2 == 0 = Alive
	| otherwise = Dead

countNeighbours :: Int -> Int -> World -> Int
countNeighbours x y w = length (filter (Alive==) [getCell i j w | i <- xs, j <- ys, i /= x || j /= y])
	where
	xs = [x-1..x+1]
	ys = [y-1..y+1]

getCell :: Int -> Int -> World -> Cell
getCell x y (World w h cs)
	| isInside x y w h = cs !! (y * w + x)
	| otherwise = Dead

isInside :: Int -> Int -> Int -> Int -> Bool
isInside x y w h = x >= 0 && y >= 0 && x < w && y < h 

evolve :: World -> World
evolve (World w h cs) = World w h [evolveCell x y (World w h cs) | x <- xs, y <- ys]
	where
	xs = [0..w-1]
	ys = [0..h-1]

evolveCell :: Int -> Int -> World -> Cell
evolveCell x y w
	| n == 2 && isAlive x y w || n == 3 = Alive
	| otherwise = Dead
	where n = countNeighbours x y w

isAlive :: Int -> Int -> World -> Bool
isAlive x y w = case c of
	Alive -> True
	Dead -> False
	where c = getCell x y w

showWorld :: World -> IO ()
showWorld (World w h cs) = undefined