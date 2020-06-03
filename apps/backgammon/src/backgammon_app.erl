%%%-------------------------------------------------------------------
%% @doc backgammon public API
%% @end
%%%-------------------------------------------------------------------
-module(backgammon_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    backgammon_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
