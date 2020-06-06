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
    backgammon_game_register_get/1,
    backgammon_game_start_with_options_invalid/1,
    backgammon_game_start/1,
    backgammon_game_start_and_join/1
]).

%%====================================================================
%% ct functions
%%====================================================================
all() ->
    [
        backgammon_game_register_get,
        backgammon_game_start_with_options_invalid,
        backgammon_game_start,
        backgammon_game_start_and_join
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
    Game = nuk_game:new("Backgammon", backgammon, 2, 2),
    ok = nuk_games:register(Game),
    {ok, Game} = nuk_games:get("Backgammon").

backgammon_game_start_with_options_invalid(_) ->
    % register game
    Game = nuk_game:new("Backgammon", backgammon, 2, 2),
    ok = nuk_games:register(Game),

    % create and login user
    User1 = nuk_user:new("User1", "Pass1"),
    ok = nuk_users:put(User1),
    {ok, UserSessionId1} = nuk_users:login("User1", "Pass1"),

    % create a game with an invalid option
    {error, invalid_options, _} =
        nuk_games:create(UserSessionId1, "Backgammon", [{foo, "bar"}]).

backgammon_game_start(_) ->
    % register game
    Game = nuk_game:new("Backgammon", backgammon, 2, 2),
    ok = nuk_games:register(Game),

    % create and login user
    User1 = nuk_user:new("User1", "Pass1"),
    ok = nuk_users:put(User1),
    {ok, UserSessionId1} = nuk_users:login("User1", "Pass1"),

    % create a game
    {ok, _GameSessionId} = nuk_games:create(UserSessionId1, "Backgammon", []).

backgammon_game_start_and_join(_) ->
    % register game
    Game = nuk_game:new("Backgammon", backgammon, 2, 2),
    ok = nuk_games:register(Game),

    % create and login user
    User1 = nuk_user:new("User1", "Pass1"),
    ok = nuk_users:put(User1),
    {ok, UserSessionId1} = nuk_users:login("User1", "Pass1"),

    % create a game
    {ok, GameSessionId} = nuk_games:create(UserSessionId1, "Backgammon", []),

    % create and login user2
    User2 = nuk_user:new("User2", "Pass2"),
    ok = nuk_users:put(User2),
    {ok, UserSessionId2} = nuk_users:login("User2", "Pass2"),

    % Join a game
    ok = nuk_games:join(GameSessionId, UserSessionId2).
