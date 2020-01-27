module ReversiAI  where 

import Reversi
import Data.Tuple

type State = ([Tile], (Reversi.Player, Reversi.Move))
data Tile = None | W | B deriving(Eq, Show)
data Node = Node State [Node] Int deriving(Eq, Show)

author :: String
author = "Casper Fredriksson de Rond"

nickname :: String
nickname = "Nolox"

initial :: Reversi.Player -> State
initial player = ([None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, W, B, None, None, None, None,
                  None, None, None, None, B, W, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None,
                  None, None, None, None, None, None, None, None, None, None],
                 (White, Pass))

think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
think state move time = (my_move, my_state)
  where
    new_state = change_state move state
    my_moves = get_valid_moves new_state
    my_move = select_move new_state
    my_state = change_state my_move new_state

test1 :: (Reversi.Move, State)
test1 = think (initial Reversi.White) Reversi.Pass 200

test2 :: Tile
test2 = get_color (initial White)

test3 :: Bool
test3 = valid_flip_dir 4 33 (initial White) False

test4 :: Bool
test4 = valid_flip_dir 2 43 (initial White) False

test5 :: Tile
test5 = get_tile 44 (initial Reversi.White)

test6 :: [Int]
test6 = get_valid_moves (initial Reversi.White)

test7 :: Node
test7 = build_tree (initial White) 2 0

get_color :: State -> Tile
get_color (_, (White, _)) = W
get_color (_, (Black, _)) = B

get_player :: State -> Reversi.Player
get_player (_, (player, _)) = player

get_move :: State -> Reversi.Move
get_move (_, (_, move)) = move

other_color :: Tile -> Tile
other_color B = W
other_color W = B

other_player :: Reversi.Player -> Reversi.Player
other_player White = Black
other_player Black = White

move_to_int :: Int -> Int
move_to_int n = ((div n 8) + 1) * 10 + (mod n 8) + 1

int_to_move :: Int -> Int
int_to_move n = ((div n 10) - 1) * 8 + (mod n 10) -1

get_tile :: Int -> State -> Tile
get_tile loc state = head (drop loc (fst state))

in_bounds :: Int -> Bool
in_bounds loc = 0 <= (int_to_move loc) && (int_to_move loc) < 64

move_dir :: Int -> Int -> Int
move_dir loc dir = case dir of
  0 -> loc - 10
  1 -> loc - 9
  2 -> loc + 1
  3 -> loc + 11
  4 -> loc + 10
  5 -> loc + 9
  6 -> loc - 1
  7 -> loc - 11

dirs :: [Int]
dirs = [0, 1, 2, 3, 4, 5, 6, 7]
  
valid_flip_dir :: Int -> Int -> State -> Bool -> Bool
valid_flip_dir dir loc state _ | (get_tile loc state) == None = False
valid_flip_dir dir loc state passed_other_color = if (get_tile loc state) == (get_color state) then passed_other_color else (valid_flip_dir dir (move_dir loc dir) state True)

get_valid_flip_dirs :: Int -> State -> [Int]
get_valid_flip_dirs loc state = filter (\dir -> valid_flip_dir dir (move_dir loc dir) state False) dirs

set_tile :: Int -> State -> State
set_tile loc state = ((take loc (fst state)) ++ ((get_color state) : tail (drop loc (fst state))), (get_player state, get_move state))

flip_dir :: Int -> Int  -> State -> State
flip_dir dir loc state | (get_tile loc state) == (get_color state) = state
flip_dir dir loc state = flip_dir dir (move_dir loc dir) (set_tile loc state)

change_state :: Reversi.Move -> State -> State
change_state Pass (list, (player, _)) = (list, (other_player player, Pass))
change_state (Move n) (list, (player, _)) = (fst (foldl func1 new_state valid_dirs), (other_player player, Move n))
                  where for_color = get_color state
                        state = (list, (player, Move n))
                        func1 = (\start_state dir -> flip_dir dir (move_dir (move_to_int n) dir) start_state)
                        new_state = (set_tile (move_to_int n) state)
                        valid_dirs = (get_valid_flip_dirs (move_to_int n) state)

board :: [Int]
board = map (move_to_int) (take 64 [0, 1 ..])

get_empty_tiles :: State -> [Int]
get_empty_tiles state = filter (\n -> (get_tile n state) == None) board

valid_move :: Int -> State -> Bool
valid_move n state | get_tile n state /= None = False
valid_move n state = if (get_valid_flip_dirs n state == []) then False else True

get_valid_moves :: State -> [Int]
get_valid_moves state = map int_to_move (filter (\n -> valid_move n state) (get_empty_tiles state))

adjecent_to_other_tile :: Int -> State -> Bool
adjecent_to_other_tile loc state = foldl (||) False (map (other_color (get_color state) ==) (map (\n -> get_tile n state) (map (move_dir loc) dirs)))

get_total_mobility :: State -> Int
get_total_mobility state = foldl (\n bool -> if bool then 1 + n else 0 + n) 0 (map (\n -> adjecent_to_other_tile n state) (get_empty_tiles state))

get_next_possible_states :: State -> [State]
get_next_possible_states state | (get_valid_moves state) == [] = [change_state Pass state]
get_next_possible_states state = map (\n -> change_state (Move n)  state) (get_valid_moves state)

build_tree :: State -> Int -> Int  -> Node
build_tree state max_depth cur_depth | max_depth == cur_depth = (Node state [] cur_depth)
build_tree state _ cur_depth | get_move state == Pass && get_valid_moves state == [] = Node state [] cur_depth
build_tree state max_depth cur_depth  = (Node state (map (\s -> build_tree s max_depth (cur_depth + 1)) (get_next_possible_states state)) cur_depth)

eval_state :: State -> Int
eval_state state = if get_move state == Pass then count_score state else get_total_mobility state

eval_tree :: Node -> Int
eval_tree (Node state list depth) = if list == [] then eval_state state else (if (mod depth 2) == 0 then  maximum (map eval_tree list) else minimum (map eval_tree list))

select_move_tree :: [Int] -> [Int] -> Int
select_move_tree list_moves list_evals | head list_evals == maximum list_evals = head list_moves
                                       | otherwise = select_move_tree (tail list_moves) (tail list_evals)

eval_move :: Int -> State -> Int
eval_move n state = eval_tree (build_tree (change_state (Move n) state) 4 1)

select_move :: State -> Reversi.Move
select_move state | get_valid_moves state == [] = Pass
select_move state = Move (select_move_tree (get_valid_moves state) (map (\n -> eval_move n state) (get_valid_moves state)))

count_score :: State -> Int
count_score state =  foldl (\n tile -> if tile == get_color state then n + 1 else if tile == None then n else n-1) 0 (map (\loc -> get_tile loc state) board)


