%% File: timeserver.erl
%% @author Alexey Rusakov <rusakovprz@rambler.ru> 
%% @copyright 2012 Alexey Rusakov

%% @doc The server time on Erlang. 
%% 
%% 
%% 
%% 

-module(timeserver).
-export([start/0, start_link/0, stop/0]).

-revision('Revision: 0.1 ').
-created('Date: 20/07/2012 17:44:57 ').
-created_by('rusakovprz@rambler.ru').

-modified('').
-modified_by('').

-behavior('gen_server').
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-export([server/0, server_loop/1]).


%====================================================================================================
%% The gen_server API (otp) 

%% Инициализация(При старте сервера)
init([]) ->
  Pid = spawn(?MODULE, server, []),
  register(tcpServer, Pid),
  ets:new(socketID, [public, named_table]),
  io:format("time server is started.~n"), 
  { ok, state }.

%% Обрабатывает забросы требуещие ответа
handle_call(Msg, _From, State) -> 
	{reply, Msg,  State}.

%% обрабатывает забросы НЕ требуещие ответа
handle_cast(stop, State) -> 
    {stop, normal, State};
handle_cast(_Msg, State) -> 
	{noreply, State}.

%% обрабатывает message's
handle_info(_Msg, State) -> 
	{noreply, State}.

%% вызывается когда супервайзер "просит" модуль остановиться
terminate(_Reason, _State) ->
  io:format("time server is stops.~n"),
  [{id, LSock}] = ets:lookup(socketID, id),
  gen_tcp:shutdown(LSock, write), % Так рекомендуется закрывать сокеты. 
  gen_tcp:shutdown(LSock, read),
  gen_tcp:close(LSock),  
	ets:delete(socketID), 
  ok.    
        
%% Метод вызывается перед тем когда нужно обновить код
code_change(_OldVersion, State, _Extra)  ->  
	{ok, State}.

%====================================================================================================
%% The server API 

%% @doc Server start
%% @spec start() -> ok
start() ->
	gen_server:start_link({local, timeserver}, ?MODULE, [], []). % [],[] - аргуменn функции init +  


%% @doc Server start
%% @spec start_link() -> {ok, Pid}
start_link() ->
  gen_server:start_link({local, timeserver}, ?MODULE, [], []).
                        																	% дополнительные опции							

%% @doc Server stops
%% @spec stop() -> {ok, Pid}
stop() ->
	gen_server:cast(echos, stop).


%====================================================================================================
%% @doc Workers.

server() ->
  {ok, LSock} = gen_tcp:listen(5678, [list, {packet, 0}, {active, false}]),
  ets:insert(socketID, {id, LSock}),
  connect(LSock).

  
connect(LSock) ->
  case gen_tcp:accept(LSock) of
    {ok, Sock} ->
      spawn(?MODULE, server_loop, [Sock]),
      connect(LSock);
    _ -> 
      error
  end.


server_loop(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, "GET"} ->
      io:format("receive 'GET'~n"),
      gen_tcp:send(Sock, gen_server:call(calendar, {cmd, getTime})),
      server_loop(Sock);
    {error,closed} ->
      {server_loop, closed};
    Data ->
      io:format("receive: ~p ~n", [Data] ),
      server_loop(Sock)             
  end.

