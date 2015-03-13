%% @copyright (C) 2015 Koji Hara. All Rights Reserved.
%%
%% @doc Sample gen_server.
-module(fun_injector_sample_gen_server).

-behaviour(gen_server).

%% Options to inject exported funs in module `fun_injector_sample_module_a' into this module.
-compile([{parse_transform, fun_injector},
          {fun_injector_extract_from, fun_injector_sample_module_a}]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Request, State) ->
    %% _A = add(self(), not_an_integer), % cause dialyzer error as expected!
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
