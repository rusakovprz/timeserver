%% File: echo_server.erl
%% @author Alexey Rusakov <rusakovprz@rambler.ru> 
%% @copyright 2012 Alexey Rusakov

%% @doc The client of an echo server on Erlang. 
%% Test job of the competitor for a position "Junior Software Developer".
%% 
%% The Echo server through Erlang port obtains data from the client and sends them reversely to the client.
%% The Client can send a signal "Operation completion" the server shall stop, and close a window of the client.
%% Having caused the stop() function, the server shall stop and close a window of the client.

-module(echo_server).
-export([start/0, stop/0, gui/0]).

-revision('Revision: 0.1 ').
-created('Date: 2012/06/11 12:49:11').
-created_by('rusakovprz@rambler.ru').

-modified('Date: 2012/07/07 14:10:00').
-modified_by('rusakovprz@rambler.ru').

-behavior('gen_server').
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%====================================================================================================
%% The gen_server API (otp) 

%% инициализация(При загрузке модуля? или при старте?)
init([]) -> 
	Pid = spawn(?MODULE, gui, []),
	register(echoServer, Pid),
	{ ok, [] }.

%% обрабатывает забросы требуещие ответа
handle_call(Msg, _From, State) -> 
	%error_loger:error_msg("handle_call(~p, ~p, ~p)~n", Msg, From, State),
	io:format("handle_call()~n"),
	{reply, Msg,  State}.

%% обрабатывает забросы НЕ требуещие ответа
handle_cast(stop, State) -> 
    io:format("handle_cast(stop, State) -> ~n"),
    {stop, normal, State};

handle_cast(Msg, State) -> 
    %error_loger:error_msg("handle_cast(~p, ~p)~n", Msg, State),	
    io:format("handle_cast(~p, ~p)~n", Msg, State),
	{noreply, State}.

%% оьрабатывает message's
handle_info(Msg, State) -> 
	%error_loger:error_msg("handle_info(~p, ~p)~n", Msg, State),
	io:format("handle_info(~p, ~p)~n", Msg, State),
	{noreply, State}.

%% вызывается когда супервайзер "просит" модуль остановиться
terminate(_Reason, _State) -> 
    gui ! {self(), {command, ["stop", 0]}},
    ok.    
     % GUI процес нужно "ГРОХАТЬ"   

        
%% Метод вызываетс перед тем когда нужно обновить код
code_change(OldVersion, State, Extra)  ->  
	error_loger:error_msg("code_change(~p, ~p, ~p)~n", OldVersion, State, Extra),
	{ok, State}.

%====================================================================================================
%% The server API 

%% @doc Server start
%% @spec start() -> ok
start() ->

	gen_server:start_link({local, echos}, echo_server, [], []). % [],[] - аргуменn функции init +  
																	% дополнительные опции							
    %Pid = spawn(?MODULE, gui, []),
	%register(echoServer, Pid).	
	%ok.

%% @doc Server break
%% @spec stop() -> ok
stop() ->
    io:format("stop() -> ~n"),
	gen_server:cast(echos, stop).
	
	%echoServer ! stop.


%% @doc Call of external GUI of the client
%% @spec gui() -> ok
gui() ->
	io:format(" erlang_echo_server:gui() ...~n"),
	
	Port = open_port({spawn, "./testgui"}, [stream, exit_status]),
	register(gui, Port),
	loop().

%====================================================================================================
%% @doc Main function of the server
loop() ->	
	io:format(" erlang_echo_server:loop() ...~n"),
	
	receive
		{Port, {data, Data}} ->

			% приняли строку от клиента и отправляем её обратно
			io:format(" erlang_echo_server:receive [~s]~n", [Data]),
			Port ! {self(), {command, [Data, 0]}},
			loop();
			
		{Port, {exit_status, Status}} ->
		
			% получен сигнал "клиентское приложение завершило работу"
			io:format(" erlang_echo_server:receive exit_status ~p  port - ~p~n", [Status, Port]);
		
		stop ->
			% получен сигнал "Завершение работы" ( функция stop() )
			io:format(" erlang_echo_server: STOP~n"),
			gui ! {self(), {command, ["stop", 0]}}, 
			% Отправляем строку "stop" клиенту, которая является сигналом завершения работы
			% и ОБЯЗАТЕЛЬНО выполняем какие либо действия.
			% Если модуль завершит свою работу - сигнал до клиента не доставиться.
			% По получению строки "stop" клиент завершит свою работу и сервер получит  
			% событие {exit_status, Status}, по которому завершит работу
			loop();
				
		 _ ->
			% Получен сигнал не оговоренный в протоколе сервера		 	
			io:format(" erlang_echo_server:receiv no defined message ~n"),
			loop()		
	end.

