%% File: gtc.erl
%% @author Alexey Rusakov <rusakovprz@rambler.ru> 
%% @copyright 2012 Alexey Rusakov

%% @doc The  - get time TCP client for testing time server on Erlang. 

-module(gtc).
-export([client/0]).

-revision('Revision: 0.1 ').
-created('Date: 07.12.2014 16:44:31 ').
-created_by('rusakovprz@rambler.ru').

-modified('').
-modified_by('').

%% Entry point.

client() ->
  case gen_tcp:connect("127.0.0.1", 5678, [list, {packet, 0}, {active, false}]) of
  {ok, Sock} ->
    ok = gen_tcp:send(Sock, "GET"),
    {ok, Data} = gen_tcp:recv(Sock, 0),
    ok = gen_tcp:close(Sock),
    Data;
  {error,econnrefused} ->
    noconnect
  end.
  
  
  


