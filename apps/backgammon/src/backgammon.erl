-module(backgammon).

-behaviour(nuk_game_engine).

-export([
    initialize/2,
    player_join/3,
    player_leave/3,
    start/2,
    turn/4,
    finish/2
]).

-spec initialize(User :: nuk_user:user(), OptionsOverride :: list()) ->
    {error, invalid_options, string()} |
    {ok, nuk_game_engine_state:state()}.
initialize(User, []) ->
    InitialBoard = #{
        1 => {white, 2},
        6 => {black, 5},
        8 => {black, 3},
        12 => {white, 5},
        13 => {black, 5},
        17 => {white, 3},
        19 => {white, 5},
        24 => {black, 2}
    },

    Username = nuk_user:get_username(User),
    StatePrivate = #{},
    StatePublic = #{board => InitialBoard},
    StatePlayers = #{Username => #{}},
    State = nuk_game_engine_state:new(StatePrivate, StatePublic, StatePlayers),
    {ok, State};
initialize(_User, OptionsOverride) when is_list(OptionsOverride) ->
    {error, invalid_options, OptionsOverride}.

player_join(User, State, _NukState) ->
    Username = nuk_user:get_username(User),
    StatePlayer = #{},
    StateNew = nuk_game_engine_state:put_player(State, Username, StatePlayer),
    {ok, StateNew}.

player_leave(User, State, NukState) ->
    case nuk_game_state:get_status(NukState) of
        % game hasn't started
        initialized ->
            case length(nuk_game_state:get_players(NukState)) of
                1 ->
                    % last player leaving, game is finished
                    % NOTE we don't remove the last player so that player can
                    % still get the game session for testing
                    %StateNew = nuk_game_engine_state:set_players(State, #{}),
                    {ok, complete, [], [], State};
                % there are more players, let this one go
                _ ->
                    Username = nuk_user:get_username(User),
                    StateNew = nuk_game_engine_state:remove_player(State, Username),
                    {ok, initialized, StateNew}
            end;
        % game has started, not allowed to leave
        _ ->
            {error, game_already_started, "Game has already started."}
    end.

start(State, NukState) ->
    {ok, await_turn,
        lists:nth(rand:uniform(2), nuk_game_state:get_players(NukState)), State}.

turn(_User, _Turn, _State, _NukState) ->
    {error, invalid_turn, "Invalid"}.

finish(_State, _NukState) ->
    ok.

