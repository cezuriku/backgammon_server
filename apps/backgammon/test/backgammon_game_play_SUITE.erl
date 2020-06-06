-module(backgammon_game_play_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).

-export([init_per_suite/1]).

-export([end_per_suite/1]).

-export([init_per_testcase/2]).

-export([end_per_testcase/2]).

%% Tests
-export([
    backgammon_game_next_player_turn/1
]).

%%====================================================================
%% ct functions
%%====================================================================
all() ->
    [
        backgammon_game_next_player_turn
    ].

init_per_suite(Config) ->
    ok = application:start(nuk),
    Config.

end_per_suite(_) ->
    application:stop(nuk),
    ok.

init_per_testcase(_, Config) ->
    % register game
    Game = nuk_game:new("Backgammon", backgammon, 2, 2),
    ok = nuk_games:register(Game),

    % create and login user
    UserName1 = "User1",
    User1 = nuk_user:new(UserName1, "Pass1"),
    ok = nuk_users:put(User1),
    {ok, UserSessionId1} = nuk_users:login("User1", "Pass1"),

    % create a game
    {ok, GameSessionId} = nuk_games:create(UserSessionId1, "Backgammon", []),

    % create and login user2
    UserName2 = "User2",
    User2 = nuk_user:new(UserName2, "Pass2"),
    ok = nuk_users:put(User2),
    {ok, UserSessionId2} = nuk_users:login("User2", "Pass2"),

    % Join a game
    ok = nuk_games:join(GameSessionId, UserSessionId2),

    % Start the game
    ok = nuk_games:start(GameSessionId, UserSessionId2),
    [
        {users, #{UserName1 => UserSessionId1, UserName2 => UserSessionId2}},
        {game, GameSessionId}
    ] ++ Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================
backgammon_game_next_player_turn(Config) ->
    Users = proplists:get_value(users, Config),
    GameSessionId = proplists:get_value(game, Config),
    UserName1 = "User1",
    UserSessionId1 = maps:get(UserName1, Users),
    UserName2 = "User2",
    UserSessionId2 = maps:get(UserName2, Users),
    % get game session data
    {ok, GameSession} = nuk_games:get_game_session(GameSessionId, UserSessionId1),
    [NextPlayer] = nuk_game_session:get_players_turn(GameSession),
    NextPlayerName = nuk_user:get_username(NextPlayer),
    case NextPlayerName of
        UserName1 ->
            ok = nuk_games:turn(GameSessionId, UserSessionId1, {[3, 4], [{1, 5}, {1, 4}]}),
            ok = nuk_games:turn(
                GameSessionId,
                UserSessionId2,
                {[3, 4], [{24, 20}, {24, 21}]}
            );
        UserName2 ->
            ok = nuk_games:turn(
                GameSessionId,
                UserSessionId2,
                {[3, 4], [{24, 20}, {24, 21}]}
            ),
            ok = nuk_games:turn(GameSessionId, UserSessionId1, {[3, 4], [{1, 5}, {1, 4}]})
    end,
    {ok, GameSession2} = nuk_games:get_game_session(GameSessionId, UserSessionId1),
    GameState2 = nuk_game_session:get_game_state(GameSession2),
    #{board := #{20 := {black, 1}, 21 := {black, 1}, 5 := {white, 1}, 4 := {white, 1}}} =
        nuk_game_engine_state:get_public(GameState2).
