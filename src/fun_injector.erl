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
                {ok, AddExports, AddClauses, AddSpecFuns} ->
                    F = fun(El={attribute, _, module, _}, Acc) ->
                                [{attribute, 11, export, AddExports}, El | Acc];
                           (El={function, Line, handle_call, 3, Clauses}, Acc) ->
                                [{function, Line, handle_call, 3, AddClauses++Clauses} | Acc];
                           (El={eof, _}, Acc) ->
                                lists:reverse(lists:reverse(AddSpecFuns)++[El], Acc);
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

gen_var_name_src(I) ->
    lists:flatten(io_lib:format("__V~p", [I])).

gen_args(Arity) ->
    [ gen_var(gen_var_name(I)) || I <- lists:seq(1, Arity) ].

gen_args_src(Arity) ->
    [ gen_var_name_src(I) || I <- lists:seq(1, Arity) ].


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

%% @doc T: tuple, ...
gen_type(T, Args) ->
    {type, 1, T, Args}.

%% {remote_type,33,
%%                          [{atom,33,fun_injector_sample_module_a},
%%                           {atom,33,state}],
gen_remote_type(Mod, Type) ->
    {remote_type, 1, [gen_atom(Mod), gen_atom(Type)]}.

%% {attribute,42,spec,
%%  {{init,1},
%%   [{type,42,'fun',
%%     [{type,42,product,[{type,42,integer,[]}]},
%%      {type,42,tuple,[{atom,42,ok},{type,42,state,[]}]}]}]}},
gen_spec(Fun, Args, ReturnType) ->
    {attribute, 1, spec,
     {{Fun, length(Args)},
      [gen_type('fun',
                [gen_type(product, Args), ReturnType])]}}.

%% @doc return AST of the code string.
parse(String) ->
    t:d({try_to_parse, String}),
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            t:d({fooo, erl_parse:parse_exprs(Tokens)}),
            case erl_parse:parse_exprs(Tokens) of
                {ok, [Expr]} ->
                    t:d({retu_fooo, Expr}),
                    Expr;
                {ok, Exprs} ->
                    error(lists:flatten(io_lib:format("multiple expr generated: ~p", [Exprs])));
                {error, Reason} ->
                    error(Reason)
            end;
        {error, Reason} ->
            error(Reason)
    end.

gen_clause(S) ->
    extract_clause_from_fun(parse(S)).

format(Format, String) ->
    lists:flatten(io_lib:format(Format, String)).

%% handle_call(_Request={add, V}, _From, State) ->
%%     t:d({call, _Request}),
%%     {V1, S1} = fun_injector_sample_module_a:add(V, State),
%%     {reply, V1, S1};
gen_handle_call_clause(Mod, Fun, Arity) ->
    Args = gen_args_src(Arity-1),
    ReqList = [atom_to_list(Fun)]++Args,
    %% ReqTuple = gen_tuple([gen_atom(Fun)]++Args),
    %% {clause, Line, Args, Guards, Body}
    %% {call, Line, {remote, Line, {atom, Line, Mod}, {atom, Line, Fun}}
    %% t:d({ff, parse("-spec foo(integer()) -> integer().")}),
    S = format("fun({~s}, _From, State) -> {RV, S1} = ~p:~p(~s), {reply, RV, S1} end.",
               [join_comma(ReqList), Mod, Fun, join_comma(Args++["State"])]),
    t:d({parsed, parse(S)}),
    gen_clause(S).


extract_clause_from_fun({'fun', _, {clauses, [Clause]}}) ->
    Clause.

%% -spec add(pid(), integer(), integer()) -> integer().
%% add(ServerRef, A, B) ->
%%     gen_server:call(ServerRef, {add, A, B}).
gen_entry_fun(Fun, Arity, {_AT, _RT}) ->
    Args = gen_args_src(Arity-1),
    ReqTuple = gen_tuple([gen_atom(Fun)]++Args),
    S = format("fun(~s) -> gen_server:call(ServerRef, {~s}) end.",
               [join_comma(["ServerRef"]++Args), join_comma([atom_to_list(Fun)]++Args)]),
    %% TODO gen spec
    %% ArgsType = [],
    %% RT = 
    %% t:d({gen_spec(Fun, ArgsType, RT)}),
    [
     gen_fun(Fun, Arity, [gen_clause(S)])
    ].

join_comma(Ss) ->
    string:join(Ss, ", ").

%% init([]) ->
%%     fun_injector_sample_module_a:init(1).
gen_init_fun(Mod, Fun, Arity, {_AT, _RT}) ->
    Args = gen_args_src(Arity),
    %% TODO gen spec
    S = format("fun([~s]) -> ~s:~s(~s) end.",
               [join_comma(Args), atom_to_list(Mod), atom_to_list(Fun), join_comma(Args)]),
    %% AT = % TODO extract from orig spec
    %% RT = gen_type(gen_remote_type(Mod, state)),
    %% t:d({spec, gen_spec(init, AT, RT)}),
    [
     gen_fun(init, Arity, [gen_clause(S)])
    ].

%% start_link(ServerName, _V0, Options) ->
%%     gen_server:start_link({local, ?MODULE}, ?MODULE, [_V0], []).
gen_start_link_fun(WithServerName, ServerMod, Fun, Arity, {_AT, _RT}) ->
    Args = gen_args_src(Arity),
    AdditionalArg = case WithServerName of
                        true -> ["ServerName"];
                        false -> []
                    end,
    StartLinkArgs = AdditionalArg++Args++["Options"],
    %% TODO gen spec
    ArgsListS = format("[~s]", [join_comma(Args)]),
    S = format("fun(~s) -> gen_server:start_link(~s) end.",
               [join_comma(StartLinkArgs),
                join_comma(AdditionalArg++[atom_to_list(ServerMod), ArgsListS, "Options"])]),
    %% Spec = ...
    F = gen_fun(start_link, length(StartLinkArgs),
            [
             gen_clause(S)
             %% gen_clause(StartLinkArgs, [],
             %%            [
             %%             gen_call(gen_server, start_link, AdditionalArg++[gen_atom(ServerMod), gen_list(Args), gen_var('Options')])
             %%            ])
            ]),
    [F].

-spec extract_funs(module(), module()) -> {ok, [{Fun::term(), Arity::integer()}], [Clause::term()], [ExportedSpecFunForm::term()]}
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
%% {specs,[{{init,1},
%%          {type,34,product,[{type,34,integer,[]}]},
%%          {type,34,tuple,
%%                [{atom,34,ok},
%%                 {remote_type,34,
%%                              [{atom,34,fun_injector_sample_module_a},
%%                               {atom,34,state},
%%                               []]}]}},
%%         {{add,2},
%%          {type,39,product,[{type,39,integer,[]},{type,39,state,[]}]},
%%          {type,39,tuple,[{type,39,integer,[]},{type,39,state,[]}]}}]}

            Specs = lists:filtermap(fun({attribute, _, spec, {MA, [{type, _, 'fun', [A, R]}]}}) -> {true, {MA, A, R}};
                                       (_) -> false
                                    end, Forms),
            t:d({specs, Specs}),
            GenFuns = [ F || F={Name, _} <- ExportedFuns, Name=/=init ],
            InitFuns = [ F || F={Name, _} <- ExportedFuns, Name=:=init ],
            case InitFuns of
                [] ->
                    {error, "No init/? fun."};
                [{InitFun, InitArity}|_] ->
                    %% pass init/? to OrigSpec
                    OrigSpec = {1, 2},
                    InitFunForm = gen_init_fun(Mod, InitFun, InitArity, OrigSpec),
                    StartLinkFunUsing3 = gen_start_link_fun(false, ServerMod, InitFun, InitArity, OrigSpec),
                    StartLinkFunUsing4 = gen_start_link_fun(true, ServerMod, InitFun, InitArity, OrigSpec),
                    t:d({init_fun, InitFunForm}),
                    t:d({genFuns, GenFuns}),
                    {Es, Cs, Fs} = lists:unzip3([ {{F,A}, gen_handle_call_clause(Mod, F, A), gen_entry_fun(F, A, OrigSpec)} || {F, A} <- GenFuns ]),
                    {ok,
                     [{start_link, InitArity+1}, {start_link, InitArity+2}|Es],
                     Cs,
                     StartLinkFunUsing3 ++ StartLinkFunUsing4 ++ InitFunForm ++ lists:append(Fs)}
            end;
        _ ->
            {error, io_lib:format("Failed to load module ~p", [Mod])}
    end.


