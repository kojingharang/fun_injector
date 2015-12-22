%% @copyright (C) 2015- kojingharang. All Rights Reserved.
%%
-module(fun_injector_tests).

-include_lib("eunit/include/eunit.hrl").

fun_injector_test_() ->
    [
     {"[HandWritten] launch server using gen_server:start_link/3",
      fun () ->
              {ok, Pid} = adder_server_handwritten:start_link(1, []),
              ?assertEqual(3, gen_server:call(Pid, {add, 2})),
              ?assertEqual(5, adder_server_handwritten:add(Pid, 2)),
              ?assertEqual(5, adder_server_handwritten:get(Pid)),
              ok
      end},
     {"[Generated] launch server using gen_server:start_link/3",
      fun () ->
              {ok, Pid} = adder_server_generated:start_link(1, []),
              ?assertEqual(3, gen_server:call(Pid, {add, 2})),
              ?assertEqual(5, adder_server_generated:add(Pid, 2)),
              ?assertEqual(5, adder_server_generated:get(Pid)),
              %% Overloaded get
              ?assertEqual(result_of_get2, adder_server_generated:get(Pid, 0)),
              ok
      end},
     {"[Generated] launch server using gen_server:start_link/4",
      fun () ->
              {ok, _Pid} = adder_server_generated:start_link({local, adder}, 1, []),
              ?assertEqual(3, gen_server:call(adder, {add, 2})),
              ?assertEqual(5, adder_server_generated:add(adder, 2)),
              ?assertEqual(5, adder_server_generated:get(adder)),
              %% Overloaded get
              ?assertEqual(result_of_get2, adder_server_generated:get(adder, 0)),
              ok
      end}
    ].
