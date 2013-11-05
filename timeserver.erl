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

-export([loop/0, client/0, server/0, get_universal_time/0]).

%====================================================================================================
%% The gen_server API (otp) 

%% инициализация(При загрузке модуля? или при старте?)
init([]) -> 
  Pid = spawn(?MODULE, loop, []),
  register(echoServer, Pid),
  { ok, [] }.

%% обрабатывает забросы требуещие ответа
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
    ok.    
        
%% Метод вызываетс перед тем когда нужно обновить код
code_change(_OldVersion, State, _Extra)  ->  
	{ok, State}.

%====================================================================================================
%% The server API 

%% @doc Server start
%% @spec start() -> ok
start() ->

	gen_server:start_link({local, echos}, echo_server, [], []). % [],[] - аргуменn функции init +  
																	% дополнительные опции							

%% @doc Server break
%% @spec stop() -> ok
stop() ->
  io:format("stop() -> ~n"),
	gen_server:cast(echos, stop).


loop() ->
  io:format("Hello world~n" ),
  ok.	

%====================================================================================================
%% @doc Test gen_tcp module

client() ->
  {ok, Sock} = gen_tcp:connect("127.0.0.1", 5678, [list, {packet, 0}, {active, false}]),
  ok = gen_tcp:send(Sock, "GET"),
  Data = client_loop(Sock),
  ok = gen_tcp:close(Sock),
  Data.


client_loop(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Data} ->
      Data;
    {error,einval} ->
      client_loop(Sock)
  end. 


server() ->
  {ok, LSock} = gen_tcp:listen(5678, [list, {packet, 0}, {active, false}]),
  {ok, Sock} = gen_tcp:accept(LSock),
  server_loop(Sock),
  ok = gen_tcp:close(Sock).


server_loop(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, "GET"} ->
      io:format("receive 'GET'~n"),
      gen_tcp:send(Sock, get_universal_time());
    {error,closed} ->  
      closed;
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

