%% vim: set ft=erlang : -*- erlang -*-
%% @copyright (C) 2015- kojingharang. All Rights Reserved.
%%
%% @doc Sample module.
-module(adder).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         init/1,
         add/2,
         get/1,
         get/2
        ]).

-export_type([
              state/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE, {
           value :: integer()
          }).

-type state() :: #?STATE{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec init(integer()) -> {ok, ?MODULE:state()}.
init(V) ->
    {ok, #?STATE{value=V}}.

-spec add(integer(), state()) -> {integer(), ?MODULE:state()}.
add(V, State=#?STATE{value=Cur}) when V >= 0 ->
    {Cur+V, State#?STATE{value=Cur+V}}.

-spec get(state()) -> {integer(), ?MODULE:state()}.
get(State=#?STATE{value=Cur}) ->
    {Cur, State}.

-spec get(term(), state()) -> {result_of_get2, ?MODULE:state()}.
get(_Any, State) ->
    {result_of_get2, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
