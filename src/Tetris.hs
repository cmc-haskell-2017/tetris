data Block = Free | Full

--Строки нашей доски
type Row = [Block]

--Все поле
type Board = [Row]

--Счет
type Score = Integer

--Координаты фигуры: первые две определяют положение фигуру
--на поле, а третья определяет ее поворот
type Coord = (Int, Int, Int)

--Состояние игры в текущий момент(разделили доску и фигуру,
--чтобы при полете фигуры не мигала вся доска, также, чтобы было более 
--оптимизировано)
type Game_state = (Board,  Figure)

--Скорость
type Speed = Float

--Для каждой фигуры свой тип, чтобы однозначно можно было 
--определить ее и тип операций над ней, например, фигуру I можно вращать
--произвольно только на расстоянии больше 4 клеток от края,
--а фигуру O на расстоянии больше 2 клеток от края
data Figure = O [Coord] |
			  I [Coord] | 
			  T [Coord] |
			  J [Coord] | L  [Coord] | 
			  S [Coord] | Z  [Coord] 
			  	deriving(Eq, Show)
	

-- =========================================
-- Generating
-- =========================================

--На вход принимается случайное число от 0 до 6, которое определяет
--Фигуру
genFigure::Int -> Figure

--Заполняем доску пустыми значениями
genEmptyBoard::Board

--Генерируем бесконечный список из случайных фигур
generateRandomFigureList:: StdGen -> [Figure]


-- =========================================
-- Moves
-- =========================================

--Поворачивает фигуру: положение фигуры в пространстве опредляется 
--двумя числами, функция смотрит, какая ей дана фигура, и вычисляет 
--расстояние до края доски и на основании этой информации поворачивает ее
--(если это можно сделать), т.е. изменяет 3 координату
turn::Figure -> Figure

--Принимает пустую доску, моделирует всю игру, после
--окончания возвращает счет
startGame::Board -> Score
	
--Переещает фигуру влево	
moveLeft::Figure -> Figure

--Перемещает фигуру вправо
moveRight::Figure -> Figure

--При нажатии клавиши "вниз" роняет фигуру 
drop::Game_state -> Game_state

-- =========================================
-- Checking rows and deleting
-- =========================================

--Смотрит, нет ли строк, которые можно удалить
checkRowsToDelete::Board -> [Bool]

--Смотрит, можно ли удаоить строку
checkRow::Row -> Bool

--Удаляет строку
deleteRow::Int -> Board -> Board

--проверяет, конец игру т.е. приземлилась ли фигура до появления на
--экране, т.е. конец игры
gameover :: Game_state -> Bool


-- =========================================
-- Drawing
-- =========================================

--Рисует доску
drawBoard::Board  -> Picture

--Рисует фигуру
drawFigure::Figure  ->  Picture

--Рисует тетрис
drawTetris ::Game_state-> Picture


-- =========================================
-- Updating
-- =========================================

--Проверяет, достигла ли нижняя часть фигуры нижней 
--границы доски или другой фигуры
collidesFloor::Game_state -> Bool

--Проверяет, не выходит ли правая или левая часть фигуры за правую или
-- левую часть доски соответственно
collidesSide::Game_state -> Bool

--Делает пустые блоки доски, на в которых находится фигура заполненными
updateBoard::Figure -> Board ->Board

--На основании прошедшего времени меняет скорость полета фигур
updateSpeed::Time -> Speed -> Speed

--Аргумент функции play, обновляет состояние тетриса
updateTetris :: Float -> Board -> Board


-- ===========================================
-- timing
-- =======================================

--Обновляет общее состояние тетриса
newTact::Figure -> Board -> Speed -> Game_state

--Застявляет фигуру постоянно падать, вызываем эту фунцию на каждом такте
newMove::Board -> Game_state


--Аргумент функции play, которя говорит, что длает каждая клавиша
handleTetris :: Event -> Game_state -> Game_state
handleTetris (Eventkey (SpecialKey KeyRight) Down _ _) = …
handleTetris (Eventkey (SpecialKey KeyLeft) Down _ _)  = …
handleTetris(Eventkey (SpecialKey KeyDown) Down _ _ ) = …
handleTetris (Evenkey (SpecialKey KeyUp) Down _ _ ) = …