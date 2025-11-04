% Sudoku Game in Prolog
% EC3272 - Artificial Intelligence Group Project
% A complete implementation of Sudoku with game mechanics

% ========================================
% SUDOKU SOLVER AND GAME IMPLEMENTATION
% ========================================

% Main predicate to start the game
start_game :-
    write('Welcome to Sudoku Game!'), nl,
    write('Choose difficulty level:'), nl,
    write('1. Easy (40 clues)'), nl,
    write('2. Medium (30 clues)'), nl,
    write('3. Hard (25 clues)'), nl,
    write('Enter choice (1-3): '),
    read(Choice),
    generate_puzzle(Choice, Grid),
    play_game(Grid).

% Generate puzzle based on difficulty
generate_puzzle(1, Grid) :-
    easy_puzzle(Grid).
generate_puzzle(2, Grid) :-
    medium_puzzle(Grid).
generate_puzzle(3, Grid) :-
    hard_puzzle(Grid).

% Sample puzzles for different difficulty levels
easy_puzzle([
    [5, 3, 0, 0, 7, 0, 0, 0, 0],
    [6, 0, 0, 1, 9, 5, 0, 0, 0],
    [0, 9, 8, 0, 0, 0, 0, 6, 0],
    [8, 0, 0, 0, 6, 0, 0, 0, 3],
    [4, 0, 0, 8, 0, 3, 0, 0, 1],
    [7, 0, 0, 0, 2, 0, 0, 0, 6],
    [0, 6, 0, 0, 0, 0, 2, 8, 0],
    [0, 0, 0, 4, 1, 9, 0, 0, 5],
    [0, 0, 0, 0, 8, 0, 0, 7, 9]
]).

medium_puzzle([
    [0, 0, 0, 6, 0, 0, 4, 0, 0],
    [7, 0, 0, 0, 0, 3, 6, 0, 0],
    [0, 0, 0, 0, 9, 1, 0, 8, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 5, 0, 1, 8, 0, 0, 0, 3],
    [0, 0, 0, 3, 0, 6, 0, 4, 5],
    [0, 4, 0, 2, 0, 0, 0, 6, 0],
    [9, 0, 3, 0, 0, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 1, 0, 0]
]).

hard_puzzle([
    [0, 0, 0, 0, 0, 0, 6, 8, 0],
    [0, 0, 0, 0, 4, 6, 0, 0, 0],
    [7, 0, 0, 0, 0, 0, 0, 0, 9],
    [0, 5, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 1, 0, 6, 0, 0, 0],
    [3, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 4, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 2, 0, 0, 0, 0],
    [0, 0, 5, 2, 0, 0, 0, 0, 0]
]).

% Main game loop
play_game(Grid) :-
    look(Grid),
    write('Commands: look, fill(Row,Col,Num), hint, check, solve, quit'), nl,
    write('Enter command: '),
    read(Command),
    process_command(Command, Grid, NewGrid),
    (Command = quit ->
        write('Thanks for playing!'), nl
    ;
        play_game(NewGrid)
    ).

% Process different commands
process_command(look, Grid, Grid) :-
    look(Grid).

process_command(fill(R, C, N), Grid, NewGrid) :-
    fill(R, C, N, Grid, NewGrid).

process_command(hint, Grid, Grid) :-
    hint(Grid).

process_command(check, Grid, Grid) :-
    check(Grid).

process_command(solve, Grid, SolvedGrid) :-
    (sudoku_solve(Grid, SolvedGrid) ->
        write('Puzzle solved!'), nl,
        look(SolvedGrid)
    ;
        write('No solution exists for this puzzle!'), nl,
        SolvedGrid = Grid
    ).

process_command(quit, Grid, Grid) :-
    write('Goodbye!'), nl.

process_command(_, Grid, Grid) :-
    write('Invalid command! Try: look, fill(Row,Col,Num), hint, check, solve, quit'), nl.

% Display the current state of the Sudoku grid
look(Grid) :-
    nl,
    write('Current Sudoku Grid:'), nl,
    write('  1 2 3   4 5 6   7 8 9'), nl,
    display_grid(Grid, 1).

display_grid([], _).
display_grid([Row|Rest], RowNum) :-
    write(RowNum), write(' '),
    display_row(Row, 1),
    nl,
    (RowNum mod 3 =:= 0, RowNum < 9 ->
        write('  -----+-------+------'), nl
    ;
        true
    ),
    NextRow is RowNum + 1,
    display_grid(Rest, NextRow).

display_row([], _).
display_row([Cell|Rest], ColNum) :-
    (Cell =:= 0 -> write('.') ; write(Cell)),
    (ColNum mod 3 =:= 0, ColNum < 9 -> write(' | ') ; write(' ')),
    NextCol is ColNum + 1,
    display_row(Rest, NextCol).

% Fill a number in a specific cell
fill(Row, Col, Num, Grid, NewGrid) :-
    (between(1, 9, Row), between(1, 9, Col), between(1, 9, Num) ->
        nth1(Row, Grid, OldRow),
        nth1(Col, OldRow, OldValue),
        (OldValue =:= 0 ->
            (is_valid_move(Grid, Row, Col, Num) ->
                replace_element(Grid, Row, Col, Num, NewGrid),
                write('Number '), write(Num), write(' placed at ('),
                write(Row), write(','), write(Col), write(')'), nl,
                (is_complete(NewGrid) ->
                    write('Congratulations! Puzzle solved!'), nl
                ;
                    true
                )
            ;
                write('Invalid move! Number '), write(Num),
                write(' conflicts with Sudoku rules.'), nl,
                NewGrid = Grid
            )
        ;
            write('Cell ('), write(Row), write(','), write(Col),
            write(') is already filled!'), nl,
            NewGrid = Grid
        )
    ;
        write('Invalid input! Row, Column, and Number must be between 1-9.'), nl,
        NewGrid = Grid
    ).

% Check if a move is valid according to Sudoku rules
is_valid_move(Grid, Row, Col, Num) :-
    \+ in_row(Grid, Row, Num),
    \+ in_column(Grid, Col, Num),
    \+ in_box(Grid, Row, Col, Num).

% Check if number is already in the row
in_row(Grid, Row, Num) :-
    nth1(Row, Grid, RowList),
    member(Num, RowList).

% Check if number is already in the column
in_column(Grid, Col, Num) :-
    extract_column(Grid, Col, Column),
    member(Num, Column).

% Check if number is already in the 3x3 box
in_box(Grid, Row, Col, Num) :-
    BoxRow is ((Row - 1) // 3) * 3 + 1,
    BoxCol is ((Col - 1) // 3) * 3 + 1,
    extract_box(Grid, BoxRow, BoxCol, Box),
    member(Num, Box).

% Extract a column from the grid
extract_column([], _, []).
extract_column([Row|RestRows], Col, [Element|RestColumn]) :-
    nth1(Col, Row, Element),
    extract_column(RestRows, Col, RestColumn).

% Extract a 3x3 box from the grid
extract_box(Grid, StartRow, StartCol, Box) :-
    EndRow is StartRow + 2,
    EndCol is StartCol + 2,
    findall(Element,
        (between(StartRow, EndRow, R),
         between(StartCol, EndCol, C),
         nth1(R, Grid, Row),
         nth1(C, Row, Element)),
        Box).

% Replace an element in the grid
replace_element(Grid, Row, Col, NewValue, NewGrid) :-
    nth1(Row, Grid, OldRow),
    replace_nth1(Col, OldRow, NewValue, NewRow),
    replace_nth1(Row, Grid, NewRow, NewGrid).

% Replace the nth element in a list
replace_nth1(1, [_|Rest], NewElement, [NewElement|Rest]).
replace_nth1(N, [Head|Tail], NewElement, [Head|NewTail]) :-
    N > 1,
    N1 is N - 1,
    replace_nth1(N1, Tail, NewElement, NewTail).

% Provide a hint to the player
hint(Grid) :-
    (find_hint(Grid, Row, Col, Num) ->
        write('Hint: Try placing '), write(Num),
        write(' at position ('), write(Row), write(','), write(Col), write(')'), nl
    ;
        write('No obvious hints available. Try using logical deduction!'), nl
    ).

% Find a hint by looking for cells with only one possible value
find_hint(Grid, Row, Col, Num) :-
    between(1, 9, Row),
    between(1, 9, Col),
    nth1(Row, Grid, RowList),
    nth1(Col, RowList, 0),  % Empty cell
    findall(N, (between(1, 9, N), is_valid_move(Grid, Row, Col, N)), PossibleNums),
    PossibleNums = [Num].  % Only one possibility

% Check if the current grid configuration is valid
check(Grid) :-
    (is_valid_grid(Grid) ->
        write('Current grid configuration is valid!'), nl
    ;
        write('Current grid has conflicts!'), nl
    ).

% Check if the entire grid is valid
is_valid_grid(Grid) :-
    valid_rows(Grid),
    valid_columns(Grid),
    valid_boxes(Grid).

% Check if all rows are valid
valid_rows([]).
valid_rows([Row|Rest]) :-
    valid_sequence(Row),
    valid_rows(Rest).

% Check if all columns are valid
valid_columns(Grid) :-
    forall(between(1, 9, Col),
           (extract_column(Grid, Col, Column),
            valid_sequence(Column))).

% Check if all 3x3 boxes are valid
valid_boxes(Grid) :-
    forall((between(1, 3, BoxRow), between(1, 3, BoxCol)),
           (StartRow is (BoxRow - 1) * 3 + 1,
            StartCol is (BoxCol - 1) * 3 + 1,
            extract_box(Grid, StartRow, StartCol, Box),
            valid_sequence(Box))).

% Check if a sequence (row, column, or box) is valid
valid_sequence(Sequence) :-
    include(\=(0), Sequence, NonZeros),
    sort(NonZeros, Sorted),
    length(NonZeros, Len),
    length(Sorted, Len).  % No duplicates

% Check if the puzzle is complete
is_complete(Grid) :-
    \+ (member(Row, Grid), member(0, Row)),  % No empty cells
    is_valid_grid(Grid).  % And valid

% ========================================
% SUDOKU SOLVER
% ========================================

% Main solving predicate
sudoku_solve(Grid, Solution) :-
    solve_sudoku(Grid, Solution).

% Solve the sudoku puzzle using backtracking
solve_sudoku(Grid, Grid) :-
    is_complete(Grid), !.

solve_sudoku(Grid, Solution) :-
    find_empty_cell(Grid, Row, Col),
    between(1, 9, Num),
    is_valid_move(Grid, Row, Col, Num),
    replace_element(Grid, Row, Col, Num, NewGrid),
    solve_sudoku(NewGrid, Solution).

% Find the first empty cell (containing 0)
find_empty_cell(Grid, Row, Col) :-
    nth1(Row, Grid, RowList),
    nth1(Col, RowList, 0), !.

% ========================================
% HOW TO RUN THE GAME
% ========================================

% Save this file as sudoku.pl and run:
% ?- [sudoku].
% ?- start_game.

% Example commands during gameplay:
% look.                    % Display current grid
% fill(1,3,4).            % Fill number 4 at row 1, column 3
% hint.                   % Get a hint
% check.                  % Check if current state is valid
% solve.                  % Solve the entire puzzle
% quit.                   % Exit the game
