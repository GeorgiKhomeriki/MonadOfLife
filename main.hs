module Main where 
import System.Environment
import System.Random
import System.Console.ANSI
import Control.Monad

data Cell = Alive | Dead
	deriving Eq

data World = World Int Int [Cell]

instance Show Cell where
	show Alive = "O"
	show Dead = " "

main :: IO ()
main = hideCursor >> cls >> loop (initWorld 30 30)

loop :: IO World -> IO ()
loop world = do
	w <- world
	showWorld w
	c <- getChar
	cls
	unless (c == 'q') $ loop (return (evolve w))

cls :: IO ()
cls = clearScreen >> setCursorPosition 0 0

initWorld :: Int -> Int -> IO World
initWorld w h = do
	cells <- replicateM (w*h) initCell
	return (World w h cells)

initCell :: IO Cell
initCell = do
	r <- randomRIO(0, 1) :: IO Int
	return (if r == 0 then Alive else Dead)

evolve :: World -> World
evolve (World w h cs) = World w h [evolveCell x y (World w h cs) | y <- ys, x <- xs]
	where
	xs = [0..w-1]
	ys = [0..h-1]

evolveCell :: Int -> Int -> World -> Cell
evolveCell x y w
	| n == 2 && isAlive x y w || n == 3 = Alive
	| otherwise = Dead
	where n = countNeighbours x y w

countNeighbours :: Int -> Int -> World -> Int
countNeighbours x y w = length (filter (True==) (map (\(i, j) -> isAlive i j w) ls))
	where
	ls = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y ]

isAlive :: Int -> Int -> World -> Bool
isAlive x y w = case c of
	Alive -> True
	Dead -> False
	where c = getCell x y w

getCell :: Int -> Int -> World -> Cell
getCell x y (World w h cs)
	| isInside x y w h = cs !! (y * w + x)
	| otherwise = Dead
	where 
	isInside x y w h = x >= 0 && y >= 0 && x < w && y < h

showWorld :: World -> IO ()
showWorld (World _ _ []) = return ()
showWorld (World w h cs) = do
	putStrLn (concatMap show (take w cs))
	showWorld (World w h (drop w cs))

