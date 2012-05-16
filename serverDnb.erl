%% ITESM CEM, December 4, 2008.
%% Erlang Source File
%% Activity: Dots and Boxes
%% Author: Gastón I. Silva Echegaray

-module(serverDnb).
-export([start/0, test/0, createGame/5, getGames/0, join/2]).
-import(board, [new/2, nextMove/3, getWinner/1]).
-define(TIME_OUT, 5 * 60 * 1000). % Five Minutes
-define(DEF_NUM_BOXES, 0).
-define(FLAG_GO, go).
-define(FLAG_WAIT, wait).
-define(NULL_PID, 0).

%%--------------------------------------------arranca el servidor
start() ->
	GamesDict = dict:new(),
    register(serverPid, spawn(fun () -> serverLoop(GamesDict) end)),
    ok.
    
%%-------------------------------------------------------------------    
test() -> ok.
    
%%-------------------------------------------------------------------
serverLoop(GamesDict) ->
    receive
		{join, From, GameName} ->
			NewDict = joinGamesDict(From, GameName, GamesDict),
			serverLoop(NewDict);
		{available, From} ->
			From ! {gamesDict, GamesDict},
			serverLoop(GamesDict);
		{addGame, From, {GameName, {GamePid, NumPlayers}}} -> 
			From ! {okGameAdded},
			NewDict = dict:append(GameName, {GamePid, NumPlayers}, GamesDict),
			serverLoop(NewDict)
    end.

getGames() ->
	ClientPid = self(),
	serverPid ! {available, ClientPid},
	receive
		{gamesDict, GamesDict} -> {gamesDict, GamesDict}
	end.

joinGamesDict(From, GameName, GamesDict) ->
	[{GamePid, NumPlayers}] = dict:fetch(GameName, GamesDict),
	IsGameFull = NumPlayers == 1,
	if
		IsGameFull -> 
			NewDict = dict:erase(GameName, GamesDict),
			GamePid ! {?FLAG_GO};
		not IsGameFull -> 
			NewDict = dict:update(GameName, fun(_)-> [{GamePid, NumPlayers-1}] end, GamesDict) % Envuelto en una lista?????
	end,
	From ! {joined, GamePid},
	NewDict.

%%-------------------------------------------------------------------
createGame(GameName, NumPlayers, {PlayerPid, PlayerName, PlayerChar}, RealWidth, RealHeigth) ->
	RPCPID = self(),
	LstPlayers = [{?DEF_NUM_BOXES, PlayerPid, PlayerName, PlayerChar}],
	{Width, Heigth, NumTurns, Board} = board:new(RealWidth, RealHeigth),
	GamePid = spawn(fun() -> gameLoop(LstPlayers, {Width, Heigth, NumTurns, Board}, ?FLAG_WAIT) end),
	serverPid ! {addGame, RPCPID, {GameName, {GamePid, NumPlayers-1}}},
	receive
        {okGameAdded} -> {gamePid, GamePid} % esto lo recibe el cliente
    end.

%%--------------------------------------------------------------------------------
join(GameName, {PlayerPid, PlayerName, PlayerChar}) ->
	io:format("Joining server...~n"),
	RPCPID = self(),
	serverPid ! {join, RPCPID, GameName}, % actualiza la lista de juegos del servidor
	receive
		{joined, GamePid} ->
			io:format("Joining game...~n"),			
			GamePid ! {addPlayer, RPCPID, {?DEF_NUM_BOXES, PlayerPid, PlayerName, PlayerChar}},% manda el mensaje a gameLoop
			receive
				{playerAdded} ->
					io:format("ok!~n"),
					{joined, GamePid}
			end
	end.


%%--------------------------------------------------------------------
gameLoop([{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T], {Width, Heigth, NumTurns, Board}, Flag) ->
	receive
		{board, From} ->
			From ! {{Width, Heigth, Board}},
			gameLoop([{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T], {Width, Heigth, NumTurns, Board}, Flag);
		{isOver, From} ->
			if
				NumTurns == 0 -> From ! {true};
				true -> From ! {false}
			end,
			gameLoop([{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T], {Width, Heigth, NumTurns, Board}, Flag);
		{nextMove, From, {ColumnK, RowK, Val}} ->
			{BoxClosed, MoveStatus, NewBoard} = board:nextMove({Width, Heigth, Board}, PlayerChar, {ColumnK, RowK, Val}),
			case MoveStatus of
				cheat ->
					From ! {moveNotMade},
					gameLoop([{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T], {Width, Heigth, NumTurns, Board}, Flag);% no cambia
				ok ->
					From ! {moveMade, {Width, Heigth, NewBoard}, BoxClosed},
					if
						BoxClosed ->
							% la misma lista, el jugador toma otro turno
							NewLstPlayers = [{NumBoxes+1, PlayerPid, PlayerName, PlayerChar} | T];
						not BoxClosed ->
							% recorrer la lista, para saber a quien le toca despues
							NewLstPlayers = T ++ [{NumBoxes, PlayerPid, PlayerName, PlayerChar}]
					end,
					gameLoop(NewLstPlayers, {Width, Heigth, NumTurns-1, NewBoard}, Flag) % si cambia
			end;
		{turn, From} -> % a quien le toca
			case Flag of
				?FLAG_GO ->
					From ! {{PlayerPid, PlayerName}};
				?FLAG_WAIT ->
					From ! {{?NULL_PID, "other players to join"}}
			end,
			gameLoop([{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T], {Width, Heigth, NumTurns, Board}, Flag);% no cambia
		{addPlayer, From, {NewNBoxes, NewPPid, NewPName, NewPChar}} ->
			NewPlayer = {NewNBoxes, NewPPid, NewPName, NewPChar},
			LstPlayers = [{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T],% la lista original
			NewLstPlayers =  LstPlayers ++ [NewPlayer],
			From ! {playerAdded},
			gameLoop(NewLstPlayers, {Width, Heigth, NumTurns, Board}, Flag);
		{winner, From} ->
			LstPlayers = [{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T],
			Winner = lists:max(LstPlayers),
			%lists:foreach(fun({NB, PPID, _PN, _PC}) -> PPID ! {Winner} end, LstPlayers)
			From ! {Winner},
			% recorrer la lista, para saber a quien le toca despues
			NewLstPlayers = T ++ [{NumBoxes, PlayerPid, PlayerName, PlayerChar}],
			gameLoop(NewLstPlayers, {Width, Heigth, NumTurns, Board}, Flag);
		{?FLAG_GO} ->
			gameLoop([{NumBoxes, PlayerPid, PlayerName, PlayerChar} | T], {Width, Heigth, NumTurns, Board}, ?FLAG_GO)% no cambia
	after ?TIME_OUT -> 
		ok % End game loop after time out.            
    end.



