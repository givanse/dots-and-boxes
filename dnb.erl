%% Dots and Boxes
%% Author: Gastón I. Silva Echegaray

-module(dnb).
-export([start/1]).
-import(plists, [foreach/2, map/2]).
-define(VERSION, "Beta 0.1").
-define(SLEEP_DELAY, 1000).
-define(MIN, 1). % minimum number of rows and columns
-define(MAX, 10). % maximum number of rows and columns
-define(PISO_MURO, 0).
-define(PISO, 1).
-define(MURO, 2).
-define(VACIO, 3).
-define(FirstRow, 0).
-define(FirstCol, 0).
-define(Diff, 1).
%%--------------------------------------------------------------------------------
start(Server) ->
    connectToServer(Server, serverDnb, test, []).

%%------------------------------------------------------------------------Menus
main_menu(Server) ->
	io:format("~nDistributed Erlang Dots&Boxes, Version ~s~n", [?VERSION]),
    io:format("(C) by GAH.~n"),
    io:format("~n===========~n"),
    io:format(" MAIN MENU~n"),    
    io:format("===========~n~n"),
    io:format("  (1) Create a new game~n"),
    io:format("  (2) Join an existing game~n"),
    io:format("  (3) Exit~n~n"),
	io:format("Select an option "),
    case read_input(1, 3) of    
        1 ->    create(Server),
				main_menu(Server);
        2 ->    join(Server),
				main_menu(Server);
        3 ->    ok
    end.

%%-------------------------------------------------------------------
create(Server) ->
	% Pide el nombre del juego    
	io:nl(), {ok, [GameName]} = io:fread('Enter the name of the game: ', "~a"),
	% Validar que el juego NO existe
	GamesDict = connectToServer(Server, serverDnb, getGames, []),   
	IsNameAKey = dict:is_key(GameName, GamesDict),
	if
		IsNameAKey ->
			io:format("A game with that name already exists.~n"),
			create(Server);
		not IsNameAKey ->
			io:format("Select the number of players "), NumPlayers = read_input(2,4),
			{PlayerPid, PlayerName, PlayerChar} = buildPlayer(),
			io:format("Board width "), RealWidth = read_input(?MIN, ?MAX),
			io:format("Board heigth "), RealHeigth = read_input(?MIN, ?MAX),
			ArgsLst = [GameName, NumPlayers, {PlayerPid, PlayerName, PlayerChar}, RealWidth, RealHeigth],
			GamePid = connectToServer(Server, serverDnb, createGame, ArgsLst),
			io:format("~nJuego creado~n"),
			turnLoop(GamePid)
    end.

connectToServer(Server, ModuleName, MethodName, ArgsLst) ->
	case rpc:call(Server, ModuleName, MethodName, ArgsLst) of
    	{badrpc, Reason} ->
			io:format("badrpc: ~n reason: ~p ~n",[Reason]),                
			error_message(lists:flatten(io_lib:format("Can't connect to ~w!", [Server])));
        {error, Message} ->
			io:format("error!!!!!!!!!~n",[]),
            error_message(Message);
		% create game
        {gamePid, GamePid} -> 
			GamePid;
		% test server
		ok ->
			main_menu(Server);
		% Join a game
		{joined, GamePid} -> 
			{joined, GamePid};
		% Available games
		{gamesDict, GamesDict} -> 
			GamesDict			
        end.

%%--------------------------------------------------------Game loop
turnLoop(GamePid) ->
	case is_game_over(GamePid) of
        true ->
			gameOver(GamePid);             
        false ->
            wait_for_turn(GamePid),% se cicla hasta que te toca tirar
            case is_game_over(GamePid) of
                true -> 
					gameOver(GamePid);
                false ->
					printBoard(GamePid),                                 
                   	% next move
                    makeAMove(GamePid),
                    turnLoop(GamePid)                    
            end            
    end.

gameOver(GamePid) ->
	printBoard(GamePid),
	GamePid ! {winner, self()},
	receive
		{{NumBoxes, _PlayerPid, PlayerName, PlayerChar}} ->
			io:format("~n~n~nThe winner is ~p ~p with a total of ~p boxes~n~n~n",[PlayerName, PlayerChar, NumBoxes])
	end.

printBoard(GamePid) ->
	{Width, Heigth, UpdatedBoard} = getBoard(GamePid),
    io:nl(), print({Width, Heigth, UpdatedBoard}).

%%-------------------------------------------------------join a game
join(Server) ->
	io:format("~n~nGetting available games...~n"),
	GamesDict = connectToServer(Server, serverDnb, getGames, []),
	Names = dict:fetch_keys(GamesDict),
	case Names of
		[] ->
			io:format("Sorry, there are no games available~n");			
		Names -> 
			io:format("Available games: ~n    ~p~n",[Names]),
			{ok, [GameName]} = io:fread('Introduce the name of the game: ', "~a"),
			% Validar que el juego SI existe
			IsNameAKey = dict:is_key(GameName, GamesDict),
			if
				IsNameAKey ->		
					{PlayerPid, PlayerName, PlayerChar} = buildPlayer(),
					%% join
					ArgsLst = [GameName, {PlayerPid, PlayerName, PlayerChar}],
					case connectToServer(Server, serverDnb, join, ArgsLst) of
						{joined, GamePid} -> 
							turnLoop(GamePid)
					end;
				not IsNameAKey -> 
					join(Server)
			end
	end.

%%---------------------------------------------------------------------------------
buildPlayer() ->
	{ok, [PlayerName]} = io:fread('Enter your name: ', "~a"),
	PlayerChar = [ hd(atom_to_list(PlayerName)) ],
	io:format("PlayerChar: ~p~n",[PlayerChar]),
	PlayerPid = self(),
	{PlayerPid, PlayerName, PlayerChar}.

%%---------------------------------------------------------------------------------
getBoard(GamePid) ->
	GamePid ! {board, self()},
	receive
		{{Width, Heigth, Board}} -> {Width, Heigth, Board}		
	end.

%%---------------------------------------------------------------------------------
makeAMove(GamePid) ->
	io:format("~nYour turn~n"),
	{Width, Heigth, _Board} = getBoard(GamePid),
	io:format("X: "), ColumnK = read_input(?FirstCol, Width-?Diff),
	io:format("Y: "), RowK = read_input(?FirstRow, Heigth-?Diff),
	io:format("V: (1 floor, 2 right wall) "), Val = read_input(?PISO, ?MURO), % 1 es piso, 2 es muro derecho
	io:format(" Box: ~p,~p  Value: ~p ~n",[ColumnK, RowK, Val]),
	GamePid ! {nextMove, self(), {ColumnK, RowK, Val}},
	receive
		{moveNotMade} ->
			io:format("~n*** That line is already taken ***~n"),
			makeAMove(GamePid);
		{moveMade, {Width, Heigth, NewBoard}, BoxClosed} ->
			print({Width, Heigth, NewBoard}),		
			if
				BoxClosed -> io:format("~nYou closed one box!!!~n");
				not BoxClosed -> ok
			end
	end.

%%--------------------------------------------------checar si el juego ya termino    
is_game_over(GamePid) ->
	GamePid ! {isOver, self()},
    receive
        {Boolean} -> Boolean
    end.

%%--------------------------------------------------chea cuando vuelves a tirar
wait_for_turn(GamePid) ->    
	GamePid ! {turn, self()},
	receive
		{{PlayerPid, PlayerName}} ->
			if
				PlayerPid == self() -> ok;% es tu turno, continua con play
				true ->
					io:format("waiting for: ~p...~n",[PlayerName]),
            		timer:sleep(?SLEEP_DELAY),
            		wait_for_turn(GamePid)
			end
	end.

%%-----------------------------------------------Print a error message
error_message(Message) ->
    io:format("~n*** ERROR ***~n"),
    io:format(Message),
    io:nl().

%%-------------------------------------------------------------------    
read_input(Min, Max) ->
    Prompt = list_to_atom(lists:flatten(
        io_lib:format("[~w, ~w]: ", [Min, Max]))),    
    case io:fread(Prompt, "~d") of
        {ok, [Result]} ->
            if 
                (Result < Min) or (Result > Max) -> 
					io:format("not a valid option~n",[]),                    
					read_input(Min, Max);  
                true -> Result
            end;
        {error, _} -> read_input(Min, Max)     
    end.


%%-----------------------------------PRINT
%%-----------------------------------Build the board as a String and print it

print({Width, Heigth, Board}) ->
	io:format("Board:~n",[]),
	% roof
	% más un espacio en blanco, para que se alinie
	NumCols = " " ++ lists:flatten([ "   " ++ integer_to_list(X) || X <- lists:seq(?FirstCol, Width)]), % encabezados de las columnas
	String = [ NumCols, "  " ++ buildRoof(Width) | buildStrRows(?FirstRow, Heigth, Board)],
	[ io:format("~p~n",[X]) || X <- String]. %% Line that prints the WHOLE string

buildRoof(?FirstCol) -> "+---+";
buildRoof(W) -> "+---" ++ buildRoof(W-1).

buildStrRows(Row, Heigth, _Dict) when Row > Heigth -> [];
buildStrRows(Row, Heigth, Board) ->
	DictRow = dict:fetch(Row, Board),
	% Hace el string de los muros
	LeftWall = integer_to_list(Row) ++ " |",  
	MuroDerecho = LeftWall ++ buildMuro(DictRow, ?FirstRow, dict:find(?FirstRow, DictRow), ""),
	% Hace el string del piso
	LeftWall2 = "  +",
	Piso = LeftWall2 ++ buildPiso(DictRow, ?FirstRow, dict:find(?FirstRow, DictRow),""),	
	[ MuroDerecho, Piso | buildStrRows(Row+1, Heigth, Board)]. 

buildMuro(_DictRow, _Col, error, String) -> String;
buildMuro(DictRow, Col, {ok, TupleValue}, String) ->
	%io:format(" TupleValue: ~p, ~p~n", [Col, TupleValue]), 
	NewString = String ++ getMuro(TupleValue),	
	buildMuro(DictRow, Col+1, dict:find(Col+1, DictRow), NewString).

getMuro({?PISO_MURO, Char}) -> " " ++ Char ++ " |";
getMuro({?PISO, Char}) -> " " ++ Char ++ "  ";
getMuro({?MURO, Char}) -> " " ++ Char ++ " |";
getMuro({?VACIO, Char}) -> " " ++ Char ++ "  ".

buildPiso(_DictRow, _Col, error, String) -> String;
buildPiso(DictRow, Col, {ok, TupleValue}, String) ->
	NewString = String ++ getPiso(TupleValue),
	buildPiso(DictRow, Col+1, dict:find(Col+1, DictRow), NewString).

getPiso({?PISO_MURO, _Char}) -> "---+";
getPiso({?PISO, _Char}) -> "---+";
getPiso({?MURO, _Char}) -> "   +";
getPiso({?VACIO, _Char}) -> "   +".
