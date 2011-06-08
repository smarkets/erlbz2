-module(erlbz2_tests).

-include_lib("eunit/include/eunit.hrl").

prop_test() ->
    ?assertEqual([], proper:module(prop_com_decom)).
