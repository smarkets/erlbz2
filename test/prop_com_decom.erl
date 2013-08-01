-module(prop_com_decom).

-export([prop_compress_decompress/0]).

-include_lib("proper/include/proper.hrl").


bin() ->
    ?SIZED(S, bin(S)).

bin(N) ->
    Chars = "ABCDEFGIJKLMNOPQRSTUVZ"
        "abcdefgijklmnopqrstuvz"
        "0123456789"
        "{}"
        "\",.",
    iolist_to_binary(
      [lists:nth(random:uniform(length(Chars)), Chars) || _ <- lists:seq(1, N)]).



prop_compress_decompress() ->
    ?FORALL(Msg, bin(),
            begin
                try
                    Msg =:= erlbz2:decompress(erlbz2:compress(Msg))
                catch
                    error:sequence_error ->
                        error_logger:info_msg("size: ~p~n~p~n",
                                              [byte_size(Msg), Msg]),
                        false
                end
            end).

