%% @copyright (C) 2015- kojingharang. All Rights Reserved.
%%
%% @doc Sample gen_server.
-module(adder_server_handwritten).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/2,
         start_link/3,
         add/2,
         get/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Start adder server.
%% ...Boring to write this.
-spec start_link(integer(), [Option]) -> Result when
      Option :: term(), % ...
      Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

%% @doc Start adder server.
%% ...Boring to write this.
-spec start_link(ServerName, integer(), [Option]) -> Result when
      ServerName :: {local, Name} | {global, GlobalName} | {via, module(), ViaName},
      Name :: atom(),
      GlobalName :: term(),
      ViaName :: term(),
      Option :: term(), % ...
      Result :: {ok, pid()} | ignore | {error, term()}.
start_link(ServerName, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, Args, Options).

%% @doc Add V to current value.
%% ...Boring to write this.
-spec add(ServerRef, integer()) -> integer() when
      ServerRef :: Name | {Name, node()} | {global, GlobalName} | {via, module(), ViaName} | pid(),
      Name :: atom(),
      GlobalName :: term(),
      ViaName :: term().
add(ServerRef, V) ->
    gen_server:call(ServerRef, {add, V}).

%% @doc Get current value.
%% ...Boring to write this.
-spec get(ServerRef) -> integer() when
      ServerRef :: Name | {Name, node()} | {global, GlobalName} | {via, module(), ViaName} | pid(),
      Name :: atom(),
      GlobalName :: term(),
      ViaName :: term().
get(ServerRef) ->
    gen_server:call(ServerRef, {get}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(V) ->
    adder:init(V).

%% @private
handle_call({add, A}, _From, State) ->
    %% ...Boring to write this.
    {ReturnValue, NewState} = adder:add(A, State),
    {reply, ReturnValue, NewState};
handle_call({get}, _From, State) ->
    %% ...Boring to write this.
    {ReturnValue, NewState} = adder:get(State),
    {reply, ReturnValue, NewState};
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
