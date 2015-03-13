%% @copyright (C) 2015 Koji Hara. All Rights Reserved.
%%
%% @doc Injects functions in some module into specified gen_server by using parse_transform
-module(fun_injector).
%% -compile(nowarn_unused_function).
%% -compile(nowarn_unused_vars).
%% -compile(nowarn_unused_record).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         parse_transform/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
%% -define(LOG(Any), ok = io:format("~p~n", [Any])).
-define(LOG(_Any), ok).

-type expr() :: erl_parse:abstract_expr().
-type spec() :: {{Fun::atom(), Arity::integer()}, ArgType::expr(), ReturnType::expr()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec parse_transform([erl_parse:abstract_form()], [compile:option()]) -> [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    ?LOG({forms, Forms}),
    ?LOG({options, _Options}),
    ServerMod = 
        case lists:filtermap(fun({attribute, _, module, M}) ->
                                     {true, M};
                                (_) -> false
                             end, Forms) of
            [M] -> M;
            _ -> error("-module not found")
        end,
    Mod = extract_target_module(Forms),
    {ok, AddExports, AddClauses, AddSpecFuns} = extract_funs(ServerMod, Mod),
    ?LOG({addSpecFuns, AddSpecFuns}),
    F = fun(El={attribute, _, module, _}, Acc) ->
                [{attribute, 11, export, AddExports}, El | Acc];
           (_El={function, Line, handle_call, 3, Clauses}, Acc) ->
                [{function, Line, handle_call, 3, AddClauses++Clauses} | Acc];
           (El={eof, _}, Acc) ->
                lists:reverse([El|AddSpecFuns], Acc);
           (El, Acc) ->
                [El | Acc]
        end,
    Forms1 = lists:reverse(lists:foldl(F, [], Forms)),
    ?LOG({new_forms, Forms1}),
    Forms1.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format(io:format(), [term()]) -> string().
format(Format, String) ->
    lists:flatten(io_lib:format(Format, String)).

-spec join_comma([string()]) -> string().
join_comma(Ss) ->
    string:join(Ss, ", ").

-spec extract_target_module([erl_parse:abstract_form()]) -> module().
extract_target_module(Forms) ->
    Mods = lists:append(lists:filtermap(
                          fun({attribute, _, compile, L}) ->
                                  {true, lists:filtermap(
                                           fun({fun_injector_extract_from, M})->{true, M};
                                              (_) -> false
                                           end, L)};
                             (_) -> false
                          end, Forms)),
    ?LOG({mods, Mods}),
    case Mods of
        [] ->
            error("fun_injector_extract_from compile option not_found");
        [Mod|_] ->
            ?LOG({mod, Mod}),
            Mod
    end.

-spec load_code(module()) -> {ok, [erl_parse:abstract_form()]} | {error, Reason::term()}.
load_code(Mod) ->
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

-spec gen_var_name_src(integer()) -> string().
gen_var_name_src(I) ->
    lists:flatten(io_lib:format("__V~p", [I])).

-spec gen_args_src(integer()) -> [string()].
gen_args_src(Arity) ->
    [ gen_var_name_src(I) || I <- lists:seq(1, Arity) ].

-spec gen_atom(atom()) -> expr().
gen_atom(Name) -> {atom, 1, Name}.
-spec gen_fun(atom(), integer(), [expr()]) -> expr().
gen_fun(Name, Arity, Clauses) -> {function, 1, Name, Arity, Clauses}.
-spec gen_type(atom(), [expr()]) -> expr().
gen_type(T, Args) -> {type, 1, T, Args}.

%% Example:
%% {attribute,42,spec,
%%  {{init,1},
%%   [{type,42,'fun',
%%     [{type,42,product,[{type,42,integer,[]}]},
%%      {type,42,tuple,[{atom,42,ok},{type,42,state,[]}]}]}]}},
-spec gen_spec(atom(), [expr()], expr()) -> erl_parse:abstract_form().
gen_spec(Fun, Args, ReturnType) ->
    {attribute, 1, spec,
     {{Fun, length(Args)},
      [gen_type('fun',
                [gen_type(product, Args), ReturnType])]}}.

%% @doc return AST of the code string.
-spec parse(string()) -> expr().
parse(String) ->
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, [Expr]} ->
                    Expr;
                {ok, Exprs} ->
                    error(lists:flatten(io_lib:format("multiple expr generated: ~p", [Exprs])));
                {error, Reason} ->
                    error(Reason)
            end;
        {error, ErrorInfo, ErrorLocation} ->
            error({ErrorInfo, ErrorLocation})
    end.

-spec gen_clause(string()) -> expr().
gen_clause(S) ->
    extract_clause_from_fun(parse(S)).

%% Example:
%% handle_call({add, V}, _From, State) ->
%%     {V1, S1} = fun_injector_sample_module_a:add(V, State),
%%     {reply, V1, S1};
-spec gen_handle_call_clause(module(), atom(), integer()) -> expr().
gen_handle_call_clause(Mod, Fun, Arity) ->
    Args = gen_args_src(Arity-1),
    ReqList = [atom_to_list(Fun)]++Args,
    S = format("fun({~s}, _From, State) -> {RV, S1} = ~p:~p(~s), {reply, RV, S1} end.",
               [join_comma(ReqList), Mod, Fun, join_comma(Args++["State"])]),
    ?LOG({parsed, parse(S)}),
    gen_clause(S).

-spec extract_clause_from_fun(expr()) -> expr().
extract_clause_from_fun({'fun', _, {clauses, [Clause]}}) ->
    Clause.

%% Example:
%% -spec add(pid(), integer(), integer()) -> integer().
%% add(ServerRef, A, B) ->
%%     gen_server:call(ServerRef, {add, A, B}).
-spec gen_entry_fun(atom(), integer(), spec()) -> [erl_parse:abstract_form()].
gen_entry_fun(Fun, Arity, {{_F, _A}, _AT, _RT}) ->
    Args = gen_args_src(Arity-1),
    S = format("fun(~s) -> gen_server:call(ServerRef, {~s}) end.",
               [join_comma(["ServerRef"]++Args), join_comma([atom_to_list(Fun)]++Args)]),
    ?LOG({at, _F, _AT, _RT}),
    ServerRefType = gen_type(union,
                             [gen_type(atom, []),
                              gen_type(tuple, [gen_type(atom, []), gen_type(atom, [])]),
                              gen_type(tuple, [gen_atom(global), gen_type(term, [])]),
                              gen_type(tuple, [gen_atom(via), gen_type(module, []), gen_type(term, [])]),
                              gen_type(pid, [])]),
    [
     gen_spec(Fun, [ServerRefType|lists:sublist(_AT, 1, length(_AT)-1)], _RT),
     gen_fun(Fun, Arity, [gen_clause(S)])
    ].

%% Example:
%% init([]) ->
%%     fun_injector_sample_module_a:init(1).
-spec gen_init_fun(module(), atom(), integer(), spec()) -> [erl_parse:abstract_form()].
gen_init_fun(Mod, Fun, Arity, {{_F, _A}, _AT, _RT}) ->
    Args = gen_args_src(Arity),
    S = format("fun([~s]) -> ~s:~s(~s) end.",
               [join_comma(Args), atom_to_list(Mod), atom_to_list(Fun), join_comma(Args)]),
    ?LOG({init_spec, _F, _AT, _RT}),
    [
     gen_spec(init, [gen_type('list', _AT)], _RT),
     gen_fun(init, Arity, [gen_clause(S)])
    ].

%% Example:
%% start_link(ServerName, _V0, Options) ->
%%     gen_server:start_link({local, ?MODULE}, ?MODULE, [_V0], []).
-spec gen_start_link_fun(boolean(), module(), atom(), integer(), spec()) -> [erl_parse:abstract_form()].
gen_start_link_fun(WithServerName, ServerMod, _InitFun, InitArity, {{_F, _A}, _AT, _RT}) ->
    Args = gen_args_src(InitArity),
    AdditionalArg = case WithServerName of
                        true -> ["ServerName"];
                        false -> []
                    end,
    StartLinkArgs = AdditionalArg++Args++["Options"],
    %% TODO gen spec SN++AT++Op -> rt of gen_server:start_link
    ArgsListS = format("[~s]", [join_comma(Args)]),
    S = format("fun(~s) -> gen_server:start_link(~s) end.",
               [join_comma(StartLinkArgs),
                join_comma(AdditionalArg++[atom_to_list(ServerMod), ArgsListS, "Options"])]),
    %% Spec = ...
    F = gen_fun(start_link, length(StartLinkArgs), [gen_clause(S)]),
    [F].

-spec find_spec(atom(), integer(), [spec()]) -> spec().
find_spec(Fun, Arity, Specs) ->
    case lists:keyfind({Fun, Arity}, 1, Specs) of
        false -> error(format("Spec for ~p/~p not found", [Fun, Arity]));
        S -> S
    end.
    

-spec extract_funs(module(), module()) ->
                          {ok,
                           FunsToExport::[{Fun::term(), Arity::integer()}],
                           [ClausesToAddToHandleCall::term()],
                           [SpecFunFormToAdd::term()]}.
extract_funs(ServerMod, Mod) ->
    case load_code(Mod) of
        {ok, Forms} ->
            ExportedFuns = lists:append(lists:filtermap(fun({attribute, _, export, FAs}) ->
                                                                {true, FAs};
                                                           (_) -> false
                                                        end, Forms)),
            %% ?LOG({loaded_mod, Forms}),
            %% ?LOG({exported, ExportedFuns}),

            Specs = lists:filtermap(fun({attribute, _, spec, {{F, A}, [{type, _, 'fun', [{type, _, product, AT}, RT]}]}})
                                       -> {true, {{F, A}, AT, RT}};
                                       (_) -> false
                                    end, Forms),
            ?LOG({specs, Specs}),
            GenFuns = [ F || F={Name, _} <- ExportedFuns, Name=/=init ],
            InitFuns = [ F || F={Name, _} <- ExportedFuns, Name=:=init ],
            case InitFuns of
                [] ->
                    error(format("No init fun in module ~p", [Mod]));
                [{InitFun, InitArity}|_] ->
                    InitOrigSpec = find_spec(InitFun, InitArity, Specs),
                    InitFunForm = gen_init_fun(Mod, InitFun, InitArity, InitOrigSpec),
                    StartLinkFunUsing3 = gen_start_link_fun(false, ServerMod, InitFun, InitArity, InitOrigSpec),
                    StartLinkFunUsing4 = gen_start_link_fun(true, ServerMod, InitFun, InitArity, InitOrigSpec),
                    ?LOG({init_fun, InitFunForm}),
                    ?LOG({genFuns, GenFuns}),
                    Gen3 = fun(F, A) ->
                                   OrigSpec = find_spec(F, A, Specs),
                                   ?LOG({search, F, OrigSpec}),
                                   {{F,A}, gen_handle_call_clause(Mod, F, A), gen_entry_fun(F, A, OrigSpec)}
                           end,
                    {Es, Cs, Fs} = lists:unzip3([ Gen3(F, A) || {F, A} <- GenFuns ]),
                    {ok,
                     [{start_link, InitArity+1}, {start_link, InitArity+2}|Es],
                     Cs,
                     StartLinkFunUsing3 ++ StartLinkFunUsing4 ++ InitFunForm ++ lists:append(Fs)}
            end;
        _ ->
            error(format("Failed to load module ~p", [Mod]))
    end.


