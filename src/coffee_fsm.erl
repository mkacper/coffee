%%%-------------------------------------------------------------------
%% @doc coffee_fsm module which implements finite state machine.
%% @end
%%%-------------------------------------------------------------------

-module(coffee_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
         espresso/0,
         americano/0,
         cappuccino/0,
         tea/0,
         time/0,
         reset/1,
         pay/1,
         cup_removed/0,
         cancel/0]).

%% gen_fsm callbacks
-export([init/1,
         terminate/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         code_change/4]).

%% gen_fsm states
-export([select/2,
         payment/2,
         remove/2]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

espresso() ->
    gen_fsm:send_event(?MODULE, {selection, espresso, 100}).

americano() ->
    gen_fsm:send_event(?MODULE, {selection, americano, 120}).

cappuccino() ->
    gen_fsm:send_event(?MODULE, {selection, cappuccino, 150}).

tea() ->
    gen_fsm:send_event(?MODULE, {selection, tea, 80}).

time() ->
    gen_fsm:sync_send_all_state_event(?MODULE, time).

reset(Pass) ->
    gen_fsm:send_all_state_event(?MODULE, {reset, Pass}).

pay(Coin) ->
    gen_fsm:send_event(?MODULE, {pay, Coin}).

cup_removed() ->
    gen_fsm:send_event(?MODULE, cup_removed).

cancel() ->
    gen_fsm:send_event(?MODULE, cancel).

%%====================================================================
%% States
%%====================================================================

select({selection, Type, Price}, _State) ->
    io:format("~p~n", [{Type, Price, select_to_payment}]),
    {next_state, payment, {Type, Price}};
select(_Other, State) ->
    io:fwrite("~p~n", [loop]),
    {next_state, select, State}.

payment({pay, Coin}, {Type, Price}) ->
    if Price - Coin =< 0 ->
            io:fwrite("~p~n", [payment_to_remove]),
            {next_state, remove, {}};
       Price - Coin > 0 ->
            io:fwrite("~p~n", [loop]),
            {next_state, payment, {Type, Price - Coin}}
    end;
payment(cancel, State) ->
    io:fwrite("~p~n", [payment_to_select]),
    {next_state, select, State};
payment(_Other, State) ->
    io:fwrite("~p~n", [loop]),
    {next_state, payment, State}.

remove(cup_removed, State) ->
    io:fwrite("~p~n", [remove_to_select]),
    {next_state, select, State};
remove(_Other, State) ->
    io:fwrite("~p~n", [loop]),
    {next_state, remove, State}.

%%====================================================================
%% Callbacks
%%====================================================================

init([]) ->
    {ok, select, {}}.

terminate(Reason, StateName, _State) ->
    {terminate, Reason, StateName}.

handle_sync_event(time, _From, StateName, State) ->
    {reply, erlang:system_time(), StateName, State}.

handle_event({reset, 12345}, _StateName, _State) ->
    {next_state, select, {}}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
