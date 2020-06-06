-module(backgammon_game_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).

-export([init_per_suite/1]).

-export([end_per_suite/1]).

-export([init_per_testcase/2]).

-export([end_per_testcase/2]).

%% Tests
-export([
    backgammon_game_register_get/1
]).

%%====================================================================
%% ct functions
%%====================================================================
all() ->
    [
        backgammon_game_register_get
    ].

init_per_suite(Config) ->
    ok = application:start(nuk),
    Config.

end_per_suite(_) ->
    application:stop(nuk),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================
backgammon_game_register_get(_) ->
    Game = nuk_game:new("Backgammon", backgammon, 1, 1),
    ok = nuk_games:register(Game),
    {ok, Game} = nuk_games:get("Backgammon").
