module Tetris where
--Test
-- =========================================
-- Types
-- =========================================

data Shape = J | L | I | S | Z | O | T
        	deriving (Eq, Show, Enum)

data Block = Free | Full

type Row = [Block]

type Board = [Row]

type Score = Integer

type Coord = (Int, Int,Int)

type Game_state = (Board,  Figure)

type Speed = Float

data Figure = O [Coord] |
			  I [Coord] | 
			  T [Coord] |
			  J [Coord] | L  [Coord] | 
			  S [Coord] | Z  [Coord] 
			  	deriving(Eq, Show)
	

-- =========================================
-- Generating
-- =========================================

gen_figure::Int -> Figure

gen_empty_board::Board

generate_random_figure_list:: StdGen -> [Figure]


-- =========================================
-- Moves
-- =========================================


turn::Figure -> Figure

start_game::Board -> Score
	
move_left::Figure -> Figure

move_right::Figure -> Figure

drop::Game_state -> Game_state

-- =========================================
-- Checking rows and deleting
-- =========================================

check_rows_to_delete::Board -> [Bool]

check_row::Row -> Bool

delete_row::Int -> Board -> Board

gameover :: Game_state -> Bool


-- =========================================
-- Drawing
-- =========================================

draw_Board::Board  -> Picture

draw_Figure::Figure  ->  Picture

draw_tetris ::Game_state-> Picture


-- =========================================
-- Updating
-- =========================================

collides_floor::Game_state -> Bool

collides_side::Game_state -> Bool

update_board::Figure -> Board ->Board

update_speed::Time -> Speed -> Speed

update_tetris :: Float -> Board -> Board


-- ===========================================
-- timing
-- =======================================

new_tact::Figure -> Board -> Speed -> Game_state

new_move::Board -> Game_state



handle_tetris :: Event -> Game_state -> Game_state
handle_tetris (Eventkey (SpecialKey KeyRight) Down _ _) = …
handle_tetris (Eventkey (SpecialKey KeyLeft) Down _ _)  = …
handle_tetris(Eventkey (SpecialKey KeyDown) Down _ _ ) = …
handle_tetris (Evenkey (SpecialKey KeyUp) Down _ _ ) = …




