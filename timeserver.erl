%% File: timeserver.erl
%% @author Alexey Rusakov <rusakovprz@rambler.ru> 
%% @copyright 2012 Alexey Rusakov

%% @doc The server time on Erlang. 
%% 
%% 
%% 
%% 

-module(timeserver).
-export([start/0, stop/0]).

-revision('Revision: 0.1 ').
-created('Date: 20/07/2012 17:44:57 ').
-created_by('rusakovprz@rambler.ru').

-modified('').
-modified_by('').

-behavior('gen_server').
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-export([loop/1, server/0, server_loop/1, get_universal_time/0]).

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
handle_call({cmd, getTime}, _From, State) -> 
  Time = get_universal_time(),
	{reply, Time,  State};
	
handle_call(Msg, _From, State) -> 
	{reply, Msg,  State}.

%% обрабатывает забросы НЕ требуещие ответа
handle_cast(stop, State) -> 
    {stop, normal, State};

handle_cast({cmd, loop, Counter}, State) -> 
    loop(Counter),
    {noreply, State};

handle_cast(_Msg, State) -> 
	{noreply, State}.

%% обрабатывает message's
handle_info(_Msg, State) -> 
	{noreply, State}.

%% вызывается когда супервайзер "просит" модуль остановиться
terminate(_Reason, _State) ->
  io:format("time server is stops.~n"), 
  ok.    
        
%% Метод вызывается перед тем когда нужно обновить код
code_change(_OldVersion, State, _Extra)  ->  
	{ok, State}.

%====================================================================================================
%% The server API 

%% @doc Server start
%% @spec start() -> ok
start() ->
	gen_server:start_link({local, echos}, ?MODULE, [], []). % [],[] - аргуменn функции init +  
                        																	% дополнительные опции							

%% @doc Server stops
%% @spec stop() -> ok
stop() ->
  [{id, LSock}] = ets:lookup(socketID, id),
  gen_tcp:shutdown(LSock, write), % Так рекомендуется закрывать сокеты. 
  gen_tcp:shutdown(LSock, read),
  gen_tcp:close(LSock),  
	ets:delete(socketID),
	gen_server:cast(echos, stop).


%% @doc реакция на на приняттое сообщение с параметром (тест поведения gen_server)
%% @spec loop(Integer) -> ok
loop(Counter) ->
  case Counter > 0 of
    true ->
      io:format("Hello world~n" ),
      loop(Counter-1);
    false ->
      ok
  end.
    	

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
      gen_tcp:send(Sock, get_universal_time()),
      server_loop(Sock);
    {error,closed} ->
      {server_loop, closed};
    Data ->
      io:format("receive: ~p ~n", [Data] ),
      server_loop(Sock)             
  end.


%% @doc Test functions get_time
%% This function returns the Universal Coordinated Time (UTC) reported by the
%% underlying operating system. Local time is returned if universal
%% time is not available.
%% Example returned data: "2013-11-5_9:42:10"

get_universal_time() -> 
  DateTime=calendar:universal_time(),
  {Date,Time} = DateTime,
  {Year,Month,Day} = Date,
  {Hours,Minutes,Seconds} = Time,
  integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ 
  integer_to_list(Day) ++ "_" ++ integer_to_list(Hours) ++ ":" ++ 
  integer_to_list(Minutes) ++ ":" ++ integer_to_list(Seconds).

