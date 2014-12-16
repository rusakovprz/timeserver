-module(sup).

-revision('Revision: 0.1 ').
-created('Date: 12.12.2014 23:21:56  ').
-created_by('rusakovprz@rambler.ru').

-modified('').
-modified_by('').

-behavior('supervisor').

-export([start_link/0, init/1, stop/0]).


%====================================================================================================
%% The superserver API. 

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_FileName) ->
  Transport = {transp, {timeserver, start_link, []},
              transient, 2000, worker, [timeserver]},
  Calendar = {cal, {tscalendar, start_link, []},
              transient, 2000, worker, [tscalendar]},
  {ok, {{one_for_all, 1, 1}, [Transport, Calendar]}}.


stop() ->
  tscalendar:stop(),
  timeserver:stop(),
  exit(whereis(?MODULE), shutdown).

