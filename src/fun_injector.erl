%% @copyright (C) 2015 Koji Hara. All Rights Reserved.
%%
%% @doc Injects functions in some module into specified gen_server by using parse_transform
-module(fun_injector).
-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_record).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         parse_transform/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
parse_transform(Forms, _Options) ->
    t:d({forms, Forms}),
    t:d({options, _Options}),
    ServerMod = 
        case lists:filtermap(fun({attribute, _, module, M}) ->
                                     {true, M};
                                (_) -> false
                             end, Forms) of
            [M] -> M;
            _ -> undefined
        end,
    case extract_target_module(Forms) of
        {ok, Mod} ->
            case extract_funs(ServerMod, Mod) of
                {ok, AddExports, AddClauses, AddFuns} ->
                    F = fun(El={attribute, _, module, _}, Acc) ->
                                [{attribute, 11, export, AddExports}, El | Acc];
                           (El={function, Line, handle_call, 3, Clauses}, Acc) ->
                                [{function, Line, handle_call, 3, AddClauses++Clauses} | Acc];
                           (El={eof, _}, Acc) ->
                                lists:reverse(lists:reverse(AddFuns)++[El], Acc);
                           (El, Acc) ->
                                [El | Acc]
                        end,
                    Forms1 = lists:reverse(lists:foldl(F, [], Forms)),
                    t:d({new_forms, Forms1}),
                    Forms1;
                {error, Reason} ->
                    forms_with_error(Forms, ServerMod, Reason)
            end;
        {error, Reason} ->
            forms_with_error(Forms, ServerMod, Reason)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
forms_with_error(Forms, ServerMod, Reason) ->
    F = fun(El={attribute, _, module, _}, Acc) ->
                [gen_error(ServerMod, Reason) | Acc];
           (El, Acc) ->
                [El | Acc]
        end,
    lists:reverse(lists:foldl(F, [], Forms)).


extract_target_module(Forms) ->
    Mods = lists:append(lists:filtermap(
                          fun({attribute, _, compile, L}) ->
                                  {true, lists:filtermap(
                                           fun({fun_injector_extract_from, M})->{true, M};
                                              (_) -> false
                                           end, L)};
                             (_) -> false
                          end, Forms)),
    t:d({mods, Mods}),
    case Mods of
        [] ->
            {error, not_found};
        [Mod|_] ->
            {ok, Mod}
    end.

load_code(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        non_existing ->
            %% do I need to compile here?
            {error,enoent};
        Path ->
            case beam_lib:chunks(Path, [abstract_code]) of
                {ok,{Mod,[{abstract_code,{raw_abstract_v1, Forms}}]}} ->
                    {ok,Forms};
                {ok,{Mod,[{abstract_code,no_abstract_code}]}} ->
                    {error,no_debug_info};
                {ok,_} ->
                    {error,unrecognised_forms};
                Error ->
                    Error
            end
    end.

gen_var_name(I) ->
    list_to_atom(lists:flatten(io_lib:format("__V~p", [I]))).

gen_args(Arity) ->
    [ gen_var(gen_var_name(I)) || I <- lists:seq(1, Arity) ].


gen_error(Mod, Reason) ->
    {error, {1, Mod, Reason}}.

gen_call(Mod, Fun, Args) ->
    {call, 1, {remote, 1, {atom, 1, Mod}, {atom, 1, Fun}}, Args}.

gen_call(Fun, Args) ->
    {call, 1, {atom, 1, Fun}, Args}.

gen_tuple(Elements) ->
    {tuple, 1, Elements}.

gen_atom(Name) ->
    {atom, 1, Name}.

gen_var(Name) ->
    {var, 1, Name}.

gen_fun(Name, Arity, Clauses) ->
    {function, 1, Name, Arity, Clauses}.

gen_clause(Args, Guards, Body) ->
    {clause, 1, Args, Guards, Body}.

gen_match(LHS, RHS) ->
    {match, 1, LHS, RHS}.

gen_cons(Car, Cdr) ->
    {cons, 1, Car, Cdr}.

gen_nil() ->
    {nil, 1}.

gen_list(Elements) ->
    lists:foldr(fun gen_cons/2, gen_nil(), Elements).

%% handle_call(_Request={add, V}, _From, State) ->
%%     t:d({call, _Request}),
%%     {V1, S1} = fun_injector_sample_module_a:add(V, State),
%%     {reply, V1, S1};
gen_handle_call_clause(Mod, Fun, Arity) ->
    Args = gen_args(Arity-1),
    ReqTuple = gen_tuple([gen_atom(Fun)]++Args),
    %% {clause, Line, Args, Guards, Body}
    %% {call, Line, {remote, Line, {atom, Line, Mod}, {atom, Line, Fun}}
    gen_clause([ReqTuple, gen_var('_From'), gen_var('State')], [],
               [gen_match(gen_tuple([gen_var('RV'), gen_var('S1')]),
                          gen_call(Mod, Fun, Args++[gen_var('State')])),
                gen_tuple([gen_atom(reply), gen_var('RV'), gen_var('S1')])
               ]).

%% -spec add(pid(), integer(), integer()) -> integer().
%% add(ServerPid, A, B) ->
%%     gen_server:call(ServerPid, {add, A, B}).
gen_entry_fun(Fun, Arity) ->
    Args = gen_args(Arity-1),
    ReqTuple = gen_tuple([gen_atom(Fun)]++Args),
    %% TODO gen spec
    gen_fun(Fun, Arity,
            [
             gen_clause([gen_var('ServerRef')|Args], [],
                        [
                         gen_call(gen_server, call, [gen_var('ServerRef'), ReqTuple])
                        ])
            ]).

%% init([]) ->
%%     fun_injector_sample_module_a:init(1).
gen_init_fun(Mod, Fun, Arity) ->
    Args = gen_args(Arity),
    %% TODO gen spec
    gen_fun(init, Arity,
            [
             gen_clause([gen_list(Args)], [],
                        [
                         gen_call(Mod, Fun, Args)
                        ])
            ]).

%% start_link(ServerName, _V0, Options) ->
%%     gen_server:start_link({local, ?MODULE}, ?MODULE, [_V0], []).
gen_start_link_fun(WithServerName, ServerMod, Fun, Arity) ->
    Args = gen_args(Arity),
    AdditionalArg = case WithServerName of
                        true -> [gen_var('ServerName')];
                        false -> []
                    end,
    StartLinkArgs = AdditionalArg++Args++[gen_var('Options')],
    %% TODO gen spec
    gen_fun(start_link, length(StartLinkArgs),
            [
             gen_clause(StartLinkArgs, [],
                        [
                         gen_call(gen_server, start_link, AdditionalArg++[gen_atom(ServerMod), gen_list(Args), gen_var('Options')])
                        ])
            ]).

-spec extract_funs(module(), module()) -> {ok, [{Fun::term(), Arity::integer()}], [Clause::term()], [ExportedFun::term()]}
                                    | {error, Reason::term()}.
extract_funs(ServerMod, Mod) ->
    case load_code(Mod) of
        {ok, Forms} ->
     %% {attribute,50,spec,
     %%            {{foo,0},
     %%             [{type,50,'fun',
     %%                    [{type,50,product,[]},{type,50,state,[]}]}]}},
     %% {function,51,foo,0,
     %%           [{clause,51,[],[],[{record,52,t_template,[]}]}]},
            %% get exported funs.
            ExportedFuns = lists:append(lists:filtermap(fun({attribute, _, export, FAs}) ->
                                                                {true, FAs};
                                                           (_) -> false
                                                        end, Forms)),
            t:d({loaded_mod, Forms}),
            t:d({exported, ExportedFuns}),
            GenFuns = [ F || F={Name, _} <- ExportedFuns, Name=/=init ],
            InitFuns = [ F || F={Name, _} <- ExportedFuns, Name=:=init ],
            case InitFuns of
                [] ->
                    {error, "No init/? fun."};
                [{InitFun, InitArity}|_] ->
                    InitFunForm = gen_init_fun(Mod, InitFun, InitArity),
                    StartLinkFunUsing3 = gen_start_link_fun(false, ServerMod, InitFun, InitArity),
                    StartLinkFunUsing4 = gen_start_link_fun(true, ServerMod, InitFun, InitArity),
                    t:d({init_fun, InitFunForm}),
                    t:d({genFuns, GenFuns}),
                    {Es, Cs, Fs} = lists:unzip3([ {{F,A}, gen_handle_call_clause(Mod, F, A), gen_entry_fun(F, A)} || {F, A} <- GenFuns ]),
                    {ok, [{start_link, InitArity+1}, {start_link, InitArity+2}|Es], Cs, [StartLinkFunUsing3, StartLinkFunUsing4, InitFunForm|Fs]}
            end;
        _ ->
            {error, io_lib:format("Failed to load module ~p", [Mod])}
    end.


