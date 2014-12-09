%% File: tscalendar.erl
%% @author Alexey Rusakov <rusakovprz@rambler.ru> 
%% @copyright 2014 Alexey Rusakov

%% @doc Date-time module for server time on Erlang. 
%% 

-module(tscalendar).
-export([start/0, stop/0]).

-revision('Revision: 0.1 ').
-created('Date: 09.12.2014 21:20:09 ').
-created_by('rusakovprz@rambler.ru').

-modified('').
-modified_by('').

-behavior('gen_server').
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%====================================================================================================
%% The gen_server API (otp) 

%% Инициализация(При старте сервера)
init([]) ->
  io:format("Calendar is started.~n"), 
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
handle_cast(_Msg, State) -> 
	{noreply, State}.

%% обрабатывает message's
handle_info(_Msg, State) -> 
	{noreply, State}.

%% вызывается когда супервайзер "просит" модуль остановиться
terminate(_Reason, _State) ->
  io:format("Calendar is stops.~n"), 
  ok.    
        
%% Метод вызывается перед тем когда нужно обновить код
code_change(_OldVersion, State, _Extra)  ->  
	{ok, State}.


%====================================================================================================
%% The server API 

%% @doc Server start
%% @spec start() -> ok
start() ->
	gen_server:start_link({local, calendar}, ?MODULE, [], []). % [],[] - аргуменn функции init +  
                        																	% дополнительные опции							

%% @doc Server stops
%% @spec stop() -> ok
stop() ->
  gen_server:cast(calendar, stop).
    	

%====================================================================================================

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

