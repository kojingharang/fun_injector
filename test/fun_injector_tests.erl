%% @copyright (C) 2015 Koji Hara. All Rights Reserved.
%%
-module(fun_injector_tests).

-include_lib("eunit/include/eunit.hrl").

fun_injector_test_() ->
    [
     {"launch server using gen_server:start_link/3",
      fun () ->
              {ok, Pid} = fun_injector_sample_gen_server:start_link(1, []),
              ?assertEqual(3, gen_server:call(Pid, {add, 2})),
              ?assertEqual(5, fun_injector_sample_gen_server:add(Pid, 2)),
              ?assertEqual(5, fun_injector_sample_gen_server:get(Pid)),
              ok
      end},
     {"launch server using gen_server:start_link/4",
      fun () ->
              {ok, _Pid} = fun_injector_sample_gen_server:start_link({local, fun_injector_sample_gen_server}, 1, []),
              ?assertEqual(3, gen_server:call(fun_injector_sample_gen_server, {add, 2})),
              ?assertEqual(5, fun_injector_sample_gen_server:add(fun_injector_sample_gen_server, 2)),
              ?assertEqual(5, fun_injector_sample_gen_server:get(fun_injector_sample_gen_server)),
              ok
      end}
    ].
