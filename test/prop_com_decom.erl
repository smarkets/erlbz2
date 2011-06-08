-module(prop_com_decom).

-export([prop_compress_decompress/0]).

-include_lib("proper/include/proper.hrl").

prop_compress_decompress() ->
    ?FORALL(Msg, binary(), Msg =:= erlbz2:decompress(erlbz2:compress(Msg))).
