%%
%% Copyright Smarkets Limited 2011.
%%
%% Portions of the code are originally:
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
-module(erlbz2).

-export([open/0, close/1,
         compress_init/3, compress/2, compress_end/1,
         decompress_init/2, decompress/2, decompress_end/1]).

-export([compress/1, decompress/1]).

-export([set_buf_size/2, get_buf_size/1, get_queue_size/1]).

-define(COMPRESS_INIT, 1).
-define(COMPRESS, 2).
-define(COMPRESS_END, 3).

-define(DECOMPRESS_INIT, 4).
-define(DECOMPRESS, 5).
-define(DECOMPRESS_END, 6).

-define(SET_BUFSZ, 7).
-define(GET_BUFSZ, 8).
-define(GET_QSIZE, 9).

open() ->
    case catch erl_ddll:load_driver(code:priv_dir(erlbz2), "erlbz2_drv") of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ ->
            case catch erl_ddll:load_driver("priv", "erlbz2_drv") of
                ok -> ok;
                {error, already_loaded} -> ok;
                E -> exit({error, E})
            end
    end,
    open_port({spawn, "erlbz2_drv"}, [binary]).

close(Z) ->
    try
        true = port_close(Z),
        receive {'EXIT', Z, _} -> ok
        after 0 -> ok
        end
    catch _:_ -> erlang:error(badarg)
    end.

compress_init(Z, BlockSz, WorkFactor) ->
    call(Z, ?COMPRESS_INIT, <<BlockSz:32, WorkFactor:32>>).

compress(Z, Data) ->
    try port_command(Z, Data) of
        true ->
            call(Z, ?COMPRESS, []),
            collect(Z)
    catch
        error:_Err ->
            flush(Z),
            erlang:error(badarg)
    end.

compress_end(Z) -> call(Z, ?COMPRESS_END, []).

decompress_init(Z, Small) ->
    call(Z, ?DECOMPRESS_INIT, <<Small:32>>).

decompress(Z, Data) ->
    try port_command(Z, Data) of
        true ->
            call(Z, ?DECOMPRESS, []),
            collect(Z)
    catch
        error:_Err ->
            flush(Z),
            erlang:error(badarg)
    end.

decompress_end(Z) -> call(Z, ?DECOMPRESS_END, []).

set_buf_size(Z, Size) -> call(Z, ?SET_BUFSZ, <<Size:32>>).
get_buf_size(Z) -> call(Z, ?GET_BUFSZ, []).
get_queue_size(Z) -> call(Z, ?GET_QSIZE, []).

compress(Binary) ->
    Z = open(),
    ok = compress_init(Z, 9, 0),
    Res = compress(Z, Binary),
    ok = compress_end(Z),
    ok = close(Z),
    iolist_to_binary(Res).

decompress(Binary) when byte_size(Binary) > 8 ->
    Z = open(),
    ok = decompress_init(Z, 0),
    Res = decompress(Z, Binary),
    ok = decompress_end(Z),
    ok = close(Z),
    iolist_to_binary(Res).

collect(Z) -> collect(Z, []).
collect(Z, Acc) ->
    receive {Z, {data, Bin}} -> collect(Z, [Bin|Acc])
    after 0 -> lists:reverse(Acc)
    end.

flush(Z) ->
    receive {Z, {data,_}} -> flush(Z)
    after 0 -> ok
    end.

call(Z, Cmd, Arg) ->
    try port_control(Z, Cmd, Arg) of
        [0|Res] -> list_to_atom(Res);
        [1|Res] ->
            flush(Z),
            erlang:error(list_to_atom(Res));
        [2,A,B,C,D] ->
            (A bsl 24)+(B bsl 16)+(C bsl 8)+D
    catch error:badarg -> erlang:error(badarg)
    end.
