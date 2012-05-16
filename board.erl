%% ITESM CEM, December 4, 2008.
%% Erlang Source File
%% Activity: Dots and Boxes
%% Author: Gastón I. Silva Echegaray

-module(board).
-export([new/2, nextMove/3]).
-import(plists, [mapreduce/2]).
%% BOXES
-define(PISO_MURO, 0).
-define(PISO, 1).
-define(MURO, 2).
-define(VACIO, 3).
-define(DefChar, " "). 
-define(NullChar, "#"). % ▒▇▓▩▣☠
-define(FirstRow, 0).
-define(FirstCol, 0).
-define(Diff, 1).

%%--------------------------------------------Start new game
new(Width, Heigth) ->
	W = Width + ?Diff,
	H = Heigth + ?Diff,
	Board = build(W, H),
	NumTurns = (Heigth * ((2 * Width) + 1)) + Width, % The total number of lines that can be draw
	{W, H, NumTurns, Board}.


%%--------------------------------------------Build the board
build(Width, Heigth) ->
	% generate one row
	FirstRow = {?FirstRow, dict:from_list( 
		[ {?FirstCol, {?PISO_MURO, ?NullChar}} | buildRow(?FirstCol+1, Width, ?MURO, ?NullChar, [])] 
	)},
	Row = dict:from_list( buildRow(?FirstCol, Width, ?VACIO, ?DefChar, []) ), % This is 1 Row
	LastRow = {Heigth, dict:from_list( 
		[ {?FirstCol, {?PISO_MURO, ?NullChar}} | buildRow(?FirstCol+1, Width, ?PISO_MURO, ?NullChar, [])]
	) },
	% generate intermidiate rows
	Rows = [ FirstRow, LastRow | [ {X, Row} || X <- lists:seq(?FirstRow + ?Diff, Heigth-1)]], % This holds all the Rows
	BoardDict = dict:from_list(Rows),
	BoardDict.

buildRow(Width, Width, ?MURO, _DChar, Lst) -> [ {Width, {?PISO_MURO, ?NullChar}} | Lst];
buildRow(Width, Width, ?VACIO, _DChar, Lst) -> [ {Width, {?PISO_MURO, ?NullChar}} | Lst];
buildRow(Width, Width, ?PISO_MURO, _DChar, Lst) -> [ {Width, {?PISO_MURO, ?NullChar}} | Lst];
buildRow(?FirstCol, Width, DValue, DChar, Lst) -> buildRow(?FirstCol+1, Width, DValue, DChar, [ {?FirstCol, {?PISO, ?NullChar}} | Lst]);
buildRow(Row, Width, DValue, DChar, Lst) -> buildRow(Row+1, Width, DValue, DChar, [ {Row, {DValue, DChar}} | Lst]).

%%--------------------------------------------Get next move
nextMove({Width, Heigth, Board}, PlayerChar, {ColumnK, RowK, Val}) ->
	{ActualValue, ActualChar} = getBox(ColumnK, RowK, Board), % Box
	{BoxClosed, MoveStatus, NewBoard} = updateBox({Width, Heigth, Board}, {ColumnK, RowK, Val}, PlayerChar, {ActualValue, ActualChar}),
	{BoxClosed, MoveStatus, NewBoard}.	

%%--------------------------------------------Update a box
 % piso, 1
updateBox({_Width, Heigth, Board}, {ColumnK, RowK, ?PISO}, PlayerChar, {ActualValue, ActualChar}) ->
	%io:format("Piso~n"),
	case ActualValue of
		?PISO_MURO -> 
			{false, cheat, Board};%es como pasar turno
		?PISO -> 
			{false, cheat, Board};%es como pasar turno
		?MURO ->
			{BoxClosed1, NewBoard1} = closeSouthBox(ColumnK, RowK, Board, Heigth, PlayerChar),
			%%% Possible box closed, Value of the box will be 0
			{BoxClosed2, NewChar} = closeBox(ColumnK, RowK, NewBoard1, ActualChar, PlayerChar),
			NewBoard2 = updateBoard(ColumnK, RowK, ?PISO_MURO, NewChar, NewBoard1),
			BoxClosed = BoxClosed1 or BoxClosed2,
			{BoxClosed, ok, NewBoard2};
		?VACIO ->
			%io:format("    Vacio~n"),
			{BoxClosed1, NewBoard1} = closeSouthBox(ColumnK, RowK, Board, Heigth, PlayerChar),
			NewBoard2 = updateBoard(ColumnK, RowK, ?PISO, ActualChar, NewBoard1),
			{BoxClosed1, ok, NewBoard2}
	end;
% muro derecho, 2
updateBox({Width, _Heigth, Board}, {ColumnK, RowK, ?MURO}, PlayerChar, {ActualValue, ActualChar}) ->
	%io:format("Muro derecho~n"),
	case ActualValue of
		?PISO_MURO -> 
			{false, cheat, Board};%es como pasar turno
		?MURO -> 
			{false, cheat, Board};%es como pasar turno
		?PISO ->
			{BoxClosed1, NewBoard1} = closeEastBox(ColumnK, RowK, Board, Width, PlayerChar),
			%%% Possible box closed, ActualValue will be 0
			{BoxClosed2, NewChar} = closeBox(ColumnK, RowK, NewBoard1, ActualChar, PlayerChar),
			NewBoard2 = updateBoard(ColumnK, RowK, ?PISO_MURO, NewChar, NewBoard1),
			BoxClosed = BoxClosed1 or BoxClosed2,
			{BoxClosed, ok, NewBoard2};
		?VACIO ->
			%io:format("    Vacio~n"),
			{BoxClosed1, NewBoard1} = closeEastBox(ColumnK, RowK, Board, Width, PlayerChar),
			NewBoard2 = updateBoard(ColumnK, RowK, ?MURO, ActualChar, NewBoard1),
			{BoxClosed1, ok, NewBoard2}
	end.

%%--------------------------------------------------------Inserts a new Box to the board
updateBoard(ColumnK, RowK, NewValue, Char, Board) ->
	DictRow = dict:fetch(RowK, Board),
	NewDictRow = dict:update(ColumnK, fun(_) -> {NewValue, Char} end, DictRow),
	NewBoard = dict:update(RowK, fun(_) -> NewDictRow end, Board),
	NewBoard.

%%--------------------------------------------------------Check if a box is now closed
closeBox(0, _RowK, _Board, ActualChar, _PlayerChar) -> {false, ActualChar};
closeBox(_ColumnK, 0, _Board, ActualChar, _PlayerChar) -> {false, ActualChar};
closeBox(ColumnK, RowK, Board, ActualChar, PlayerChar) -> 
	{NorthVal, _Ch1} = getBox(ColumnK, RowK-1, Board),
	{WestVal, _Ch2} = getBox(ColumnK-1, RowK, Board),
	IsBoxClosed = is01(NorthVal) and is02(WestVal),
	if
		IsBoxClosed -> 
			{true, PlayerChar};
		not IsBoxClosed -> 
			{false, ActualChar} %keeps the current char
	end.

closeSouthBox(_ColumnK, RowK, Board, Heigth, _PlayerChar) when Heigth-1==RowK -> {false, Board};
closeSouthBox(ColumnK, RowK, Board, _Heigth, PlayerChar) ->
	%io:format("*south~n"),	
	{SouthVal, _Ch1} = getBox(ColumnK, RowK+1, Board),
	%io:format("**southwest~n"),	
	{SouthWestVal, _Ch2} = getBox(ColumnK-1, RowK+1, Board),
	%io:format("***closeSouthBox: SV ~p, SWV ~p~n",[SouthVal, SouthWestVal]),
	IsSouthBoxClosed = is0(SouthVal) and is02(SouthWestVal),
	if
		IsSouthBoxClosed ->
			%io:format("update board~n"), 
			NewBoard = updateBoard(ColumnK, RowK+1, SouthVal, PlayerChar, Board),
			{true, NewBoard};
		not IsSouthBoxClosed ->
			%io:format("el box no esta cerrado ~n"),
			{false, Board} % The box is not closed
	end.

closeEastBox(ColumnK, _RowK, Board, Width, _PlayerChar) when Width-1==ColumnK -> {false, Board};
closeEastBox(ColumnK, RowK, Board, _Width, PlayerChar) -> 
	{EastVal, _Ch1} = getBox(ColumnK+1, RowK, Board),
	{NorthEastVal, _Ch2} = getBox(ColumnK+1, RowK-1, Board),
	IsEastBoxClosed = is0(EastVal) and is01(NorthEastVal),
	if
		IsEastBoxClosed -> 
			NewBoard = updateBoard(ColumnK+1, RowK, EastVal, PlayerChar, Board),
			{true, NewBoard};
		not IsEastBoxClosed -> 
			{false, Board} % The box is not closed
	end.

is0(0) -> true;
is0(_Val) -> false.

is01(0) -> true;
is01(1) -> true;
is01(_Val) -> false.

is02(0) -> true;
is02(2) -> true;
is02(_Val) -> false.

%%--------------------------------------------Get a box from the board
getBox(ColumnK, RowK, Board) -> 
	DictRow = dict:fetch(RowK, Board),
	{Value, Char} = dict:fetch(ColumnK, DictRow),
	{Value, Char}.
	





