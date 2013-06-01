module Main where 
import System.Environment
import System.Random
import System.Console.ANSI
import System.Timeout
import System.IO
import Control.Monad
import Control.DeepSeq

data Cell = Alive | Dead
    deriving Eq

data World = World Int Int [Cell]

instance Show Cell where
    show Alive = "O"
    show Dead  = " "

-- given width and height as arguments initializes a random World
-- and starts the main loop
main :: IO ()
main = do 
    args <- getArgs
    if length args /= 2 then putStrLn "Usage: life width height"
    else do 
        hSetBuffering stdin NoBuffering
        hideCursor
        cls
        loop $ initWorld (read $ head args) (read $ args !! 1)

-- displays the World, evolves it and checks for input
loop :: IO World -> IO ()
loop world = do
    w @ (World sw sh cs) <- world
    setSGR [SetColor Foreground Vivid Cyan]
    showWorld w
    setSGR []
    showInfo cs
    input <- timeout 50000 getChar
    case input of
        Just i | i == 'q'  -> setSGR [] >> showCursor
               | i == 'r'  -> initWorld sw sh >>= step
               | otherwise -> step w
        Nothing -> step w
    where step w = cls >> loop (return $ evolve w)

-- clears the screen and resets cursor position
cls :: IO ()
cls = clearScreen >> setCursorPosition 0 0

-- given width and height, returns a random World
initWorld :: Int -> Int -> IO World
initWorld w h = do
    cells <- replicateM (w * h) initCell
    return $ World w h cells

-- returns a random Cell
initCell :: IO Cell
initCell = do
    r <- randomRIO(0, 1) :: IO Int
    return $ if r == 0 then Alive else Dead

-- given a World, return the World's next (evolved) state
evolve :: World -> World
evolve world @ (World w h cs) = World w h [evolveCell x y world | y <- ys, x <- xs]
    where
    xs = [0..w-1]
    ys = [0..h-1]

-- given the coordinates of a cell in the given world returns that Cell's next state
evolveCell :: Int -> Int -> World -> Cell
evolveCell x y w
    | n == 2 && isAlive x y w || n == 3 = Alive
    | otherwise                         = Dead
    where n = countNeighbours x y w

-- returns the number of adjacent living Cells to the Cell at (x, y)
countNeighbours :: Int -> Int -> World -> Int
countNeighbours x y w = length . filter (True==) . map (\(i, j) -> isAlive i j w) $ ls
    where ls = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y ]

-- checks whether the Cell at (x, y) is dead or alive
isAlive :: Int -> Int -> World -> Bool
isAlive x y w = case c of
    Alive -> True
    Dead  -> False
    where c = getCell x y w

-- returns the Cell that is on (x, y) in the given World
getCell :: Int -> Int -> World -> Cell
getCell x y (World w h cs)
    | isInside  = cs !! (y * w + x)
    | otherwise = Dead
    where isInside = x >= 0 && y >= 0 && x < w && y < h

-- display the given World
showWorld :: World -> IO ()
showWorld w = putStrLn $!! readWorld w

-- takes a World and returns it's String representation 
readWorld :: World -> String
readWorld (World _ _ []) = []
readWorld (World w h cs) = concatMap show (take w cs) 
    ++ "\n" ++ readWorld (World w h $ drop w cs)

-- given the list of current Cells, display's some information
showInfo :: [Cell] -> IO ()
showInfo cs = putStrLn ("(q - quit)     (r - reset)     live cells: " 
    ++ (show . length $ filter (Alive==) cs))

