-module(test).
-export([test_all/0]).

test_all() ->
   code:add_path("../ebin"),
   code:add_path("../lib/erlzmq2/ebin"),
   {ok, _Pid} = breath_mq:start_link(),
   ok = breath:send_msg("test").