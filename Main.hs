module Main where

data Player = P1 | P2 | Open deriving (Show, Eq)

--display player name
showPlayer :: Player -> [Char]
showPlayer x | x == P1 = "Player 1" | x == P2 = "Player 2" | otherwise = "Unidentified"

--default board
defaultBoard :: [[Player]]
defaultBoard = [[Open, Open, Open],[Open, Open, Open],[Open, Open, Open]]

--start of the game
main :: IO ()
main = putStrLn "Welcome to tic-tac-toe " >> putStrLn "First player is x and second player is o " >>
    gameLoop2 P1 defaultBoard

--game loop
gameLoop2 :: Player -> [[Player]] -> IO ()
gameLoop2 player board = do {
    printBoard2 (displayBoard board) ;
    newBoard <- place player board ;
    if win player newBoard then putStrLn (showPlayer player ++ " wins.") >> pure ()
        else if filled newBoard then putStrLn "Aww... no one wins." >> pure ()
            else gameLoop2 (flip player) newBoard
}
    where
        flip :: Player -> Player
        flip x | x == P1 = P2 | otherwise = P1

printBoard2 :: [[Char]] -> IO ()
printBoard2 xs = (putStrLn . (++) "----------\n") (foldr (\ x -> (++) (printRow x ++ "\n----------\n")) [] xs)
    where
        printRow :: [Char] -> [Char]
        printRow xs =  '|' : foldr (\ x -> (++) (x : " |")) [] xs
        
playerToChar :: Player -> Char
playerToChar x | x == P1 = 'X' | x == P2 = 'O' | otherwise = ' '

--rec
recDisplayBoard :: [Player] -> [Char]
recDisplayBoard = foldr (\ x -> (++) [playerToChar x]) []

--display the board
displayBoard :: [[Player]] -> [[Char]]
displayBoard = foldr (\ x -> (++) [recDisplayBoard x]) []

--test to see if valid position
valid :: Int -> Int -> [[Player]] -> Bool
valid x y board = let u = x - 1 in let v = y - 1 in
        not (u < 0 || v < 0 || u > 2 || v > 2) &&  board !! v !! u == Open

--place chip on the board
place :: Player -> [[Player]] -> IO [[Player]]
place player board = do {
    putStrLn ("Place a piece onto the board " ++ showPlayer player ++ "\nPlace the xy direction: ") ;
    putStrLn "x: " >> (read <$> getLine) >>= (\ x -> putStrLn "y: " >> (read <$> getLine) >>= (\ y -> if valid x y board
        then return (recPlace x y player board) else putStrLn "Invalid position." >> place player board))
    }

--place onto the board
recPlace :: Int -> Int -> Player -> [[Player]] -> [[Player]]
recPlace x y player board = let u = x - 1 in let v = y - 1 in let row = board !! v in
    let newRow = take u row ++ [player] ++ drop (u + 1) row in take v board ++ [newRow] ++ drop (v + 1) board

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
horiWin player (x:xs) | player == x = horiWin player xs | otherwise = False

dWin :: Player -> [[Player]] -> Bool
dWin player board = rDiagonalWin 0 1 player board || rDiagonalWin 2 (-1) player board

--check to see diagonal wins
rDiagonalWin :: Int -> Int -> Player -> [[Player]] -> Bool
rDiagonalWin _ _ _          [] = True
rDiagonalWin i d player (x:xs) | player == (x !! i) = rDiagonalWin (i + d) d player xs | otherwise = False

--check to see win
win :: Player -> [[Player]] -> Bool
win player board = mHoriWin player board || dWin player board || mHoriWin player (transpose board)

--check to see if board is filled
filled :: [[Player]] -> Bool
filled []     = True
filled (x:xs) = rowFill x && filled xs
              where
                  rowFill :: [Player] -> Bool
                  rowFill [] = True
                  rowFill (x:xs) | x == Open = False | otherwise = rowFill xs