-module(jpass_util).

-export([send_req/2, send_res/3, get_datetime_str/0, encode/1, decode/1, close_tcp_socket/1, close_udp_socket/1]).

-define(JPASS_SERVER, jpass_server).
-define(JPASS_REPEATER, jpass_repeater).

send_req(Cmd, Params) ->
    {ok, ServerRef} = application:get_env(server_ref),
    CipherParams = encode(Params),
    gen_server:cast(ServerRef, {{erlang:self(), undefined}, Cmd, CipherParams}).

send_res({FromPid, RepeaterPid}, Cmd, Params) ->
    CipherParams = encode(Params),
    TargetPid = case RepeaterPid of
        undefined -> FromPid;
        _ -> RepeaterPid
    end,
    TargetPid ! {{FromPid, RepeaterPid}, Cmd, CipherParams}.

get_datetime_str() ->
    {{Year, Month, Day}, {Hour, Minutes, Second}} = erlang:localtime_to_universaltime(erlang:localtime()),
    io_lib:format("[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B]", [Year, Month, Day, Hour, Minutes, Second]).

encode(PlainTerm) ->
    Key = rand:uniform(255),
    Inc = rand:uniform(7),

    PlainBinary = erlang:term_to_binary(PlainTerm),
    PlainList = erlang:binary_to_list(PlainBinary),
    CipherList = encode_bytes(PlainList, [], Key, Inc),

    {{Key, Inc}, CipherList}.

encode_bytes(PlainList, CipherList, Key, Inc) ->
    case PlainList of
        [PlainByte] ->
            CipherByte = encode_byte(PlainByte, Key), 
            CipherList2 = [CipherByte | CipherList],
            lists:reverse(CipherList2);
        [PlainByte | PlainList2] ->
            CipherByte = encode_byte(PlainByte, Key), 
            CipherList2 = [CipherByte | CipherList],
            Key2 = increase_key(Key, Inc),
            encode_bytes(PlainList2, CipherList2, Key2, Inc)
    end.

encode_byte(PlainByte, Key) ->
    CipherByte = PlainByte + Key,
    if CipherByte >= 256 -> CipherByte - 256;
                    true -> CipherByte
    end.

decode({{Key, Inc}, CipherList}) ->
    PlainList = decode_bytes(CipherList, [], Key, Inc),
    PlainBinary = erlang:list_to_binary(PlainList),
    PlainTerm = erlang:binary_to_term(PlainBinary),
    PlainTerm.

decode_bytes(CipherList, PlainList, Key, Inc) ->
    case CipherList of
        [CipherByte] ->
            PlainByte = decode_byte(CipherByte, Key), 
            PlainList2 = [PlainByte | PlainList],
            lists:reverse(PlainList2);
        [CipherByte | CipherList2] ->
            PlainByte = decode_byte(CipherByte, Key), 
            PlainList2 = [PlainByte | PlainList],
            Key2 = increase_key(Key, Inc),
            decode_bytes(CipherList2, PlainList2, Key2, Inc)
    end.

decode_byte(CipherByte, Key) ->
    PlainByte = CipherByte - Key,
    if PlainByte < 0 -> PlainByte + 256;
                true -> PlainByte
    end.

increase_key(Key, Inc) ->
    Key2 = Key + Inc,
    if Key2 >= 256 -> Key2 - 256;
              true -> Key2
    end.

close_tcp_socket(undefined) ->
    ok;
close_tcp_socket(Socket) ->
    gen_tcp:close(Socket).

close_udp_socket(undefined) ->
    ok;
close_udp_socket(Socket) ->
    gen_udp:close(Socket).

