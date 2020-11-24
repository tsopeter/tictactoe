module Main where

data Player = P1 | P2 | Open deriving (Show, Eq)

--display player name
showPlayer :: Player -> [Char]
showPlayer P1 = "player 1"
showPlayer P2 = "player 2"
showPlayer _  = "unidenitifed"

--default board
defaultBoard :: [[Player]]
defaultBoard = [
    [Open, Open, Open],
    [Open, Open, Open],
    [Open, Open, Open]
    ]

--boardDimensions
dx :: Int
dx = 3 

dy :: Int
dy = 3

du :: Int
du = dx - 1

dv :: Int
dv = dy - 1

--start of the game
main :: IO ()
main = do {
    putStrLn "Welcome to tic-tac-toe ";
    putStrLn "First player is x and second player is o ";
    gameLoop P2 defaultBoard ;
    }

--game loop
gameLoop :: Player -> [[Player]] -> IO ()
gameLoop player board = do {
    printBoard (displayBoard board) ;
            if player == P2
                then do {
                        newBoard <- place P1 board ;
                        if win P1 newBoard
                            then putStrLn "Player 1 wins" >> pure ()
                            else if filled newBoard
                                then putStrLn "Aww... no one wins." >> pure ()
                                else gameLoop P1 newBoard ;
                    }
                else do {
                        newBoard <-  place P2 board ;
                        if win P2 newBoard
                            then putStrLn "Player 2 wins" >> pure ()
                            else if filled newBoard
                                then putStrLn "Aww... no one wins." >> pure ()
                                else gameLoop P2 newBoard ;
                    }
    }


printBoard :: [[Char]] -> IO ()
printBoard []     = pure ()
printBoard (x:xs) | null xs = putStrLn (printRow x)
                  | otherwise = putStrLn (printRow x) >> putStrLn "---------" >> printBoard xs
    where
        printRow :: [Char] -> [Char]
        printRow []     = []
        printRow (x:xs) | null xs = [x]
                        | otherwise = [x] ++ " | " ++ printRow xs 

playerToChar :: Player -> Char
playerToChar P1 = 'X'
playerToChar P2 = 'O' 
playerToChar _  = ' '

--rec
recDisplayBoard :: [Player] -> [Char]
recDisplayBoard = foldr (\ x -> (++) [playerToChar x]) []

--display the board
displayBoard :: [[Player]] -> [[Char]]
displayBoard = foldr (\ x -> (++) [recDisplayBoard x]) []

--test to see if valid position
valid :: Int -> Int -> [[Player]] -> Bool
valid x y board = let u = x - 1 in
    let v = y - 1 in
        not (u < 0 || v < 0 || u > du || v > dv)
            &&  board !! v !! u == Open

getInt :: IO Int
getInt = read <$> getLine

--place chip on the board
place :: Player -> [[Player]] -> IO [[Player]]
place player board = do {
    putStrLn ("Place a piece onto the board " ++ showPlayer player ) ;
    putStrLn "Place the x direction: " ;
    x <- getInt ;
    putStrLn "Place the y direction: " ;
    y <- getInt ;
    if valid x y board
        then return (recPlace x y player board) ;
        else putStrLn "Invalid position. " >> place player board ;
    }

--place onto the board
recPlace :: Int -> Int -> Player -> [[Player]] -> [[Player]]
recPlace x y player board = let u = x - 1 in
    let v = y - 1 in
        let row = board !! v in
            let newRow = take u row ++ [player] ++ drop (u + 1) row in
                take v board ++ [newRow] ++ drop (v + 1) board

--transpose a matrix
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

--main horizontal wins
mHoriWin :: Player-> [[Player]] -> Bool
mHoriWin player board = horiWin player (head board) || horiWin player (board !! 1) || horiWin player (board !! 2) 

--check horizontal wins
horiWin :: Player -> [Player] -> Bool
horiWin _      []     = True
horiWin player (x:xs) | player == x = horiWin player xs
                      | otherwise = False

--check vertical wins
vertWin :: Player -> [[Player]] -> Bool
vertWin player board = let newboard = transpose board in mHoriWin player newboard

dWin :: Player -> [[Player]] -> Bool
dWin player board = rDiagonalWin 0 1 player board || rDiagonalWin 2 (-1) player board

--check to see diagonal wins
rDiagonalWin :: Int -> Int -> Player -> [[Player]] -> Bool
rDiagonalWin _ _ _          [] = True
rDiagonalWin i d player (x:xs) | player == (x !! i) = rDiagonalWin (i + d) d player xs
                               | otherwise = False

--check to see win
win :: Player -> [[Player]] -> Bool
win player board = mHoriWin player board || vertWin player board || dWin player board

--check to see if board is filled
filled :: [[Player]] -> Bool
filled []     = True
filled (x:xs) | rowFill x = filled xs
              | otherwise = False
              where
                  rowFill :: [Player] -> Bool
                  rowFill [] = True
                  rowFill (x:xs) | x == Open = False
                                 | otherwise = rowFill xs
