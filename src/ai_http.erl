-module(ai_http).
-include("aihttp.hrl").

-export([content_length/1,etag/1,last_modified/1]).
-export([accept_ranges/1,content_range/1,range/1]).
-export([content_encoding/1]).
-export([encode_body/2,decode_body/2]).

-export([add_if_none_match/2,add_if_modified_since/2]).
-export([add_content_encoding/2,add_content_length/2]).

headers(H) when erlang:is_map(H) -> maps:to_list();
headers(H) -> H.

content_length(Headers)->
    H = headers(Headers),
    case proplists:get_value(?CONTENT_LENGTH,H) of
        undefined -> undefined;
        Length -> erlang:binary_to_integer(Length)
    end.
etag(Headers)->
    H = headers(Headers),
    proplists:get_value(?ETAG,H).
last_modified(Headers)->
    H = headers(Headers),
    proplists:get_value(?LAST_MODIFIED,H).
accept_ranges(Headers)->
    H = headers(Headers),
    case proplists:get_value(?ACCEPT_RANGES,H) of 
        undefined -> false;
        Type -> 
            case Type of 
                ?BYTES -> true;
                ?NONE -> false
            end
    end.
content_range(Headers)->
    H = headers(Headers),
    case proplists:get_value(?CONTENT_RANGE,H) of 
        undefined -> undefined;
        ContentRange -> 
            cow_http_hd:parse_content_range(ContentRange)
    end.               
range(Headers)->
    H = headers(Headers),
    case proplists:get_value(?RANGE,H) of 
        undefined -> undefined;
        Range ->
            cow_http_hd:parse_range(Range)
    end.
content_encoding(Headers)-> proplists:get_value(?CONTENT_ENCODING,Headers).

-spec decode_body(atom(),binary())-> binary() | {ok,binary()}.
decode_body(<<"gzip">>,Body)->{ok,zlib:gunzip(Body)};
decode_body(_,Body) -> Body.
-spec encode_body(atom(),binary())-> binary() | {ok,binary()}.
encode_body(gzip,Body) -> {ok,zlib:gzip(Body)};
encode_body(_,Body) -> Body.

add_if_none_match(undefined,Headers)-> Headers;
add_if_none_match(Etag,Headers)->
    Filterd = lists:filter(fun({Key,_V})-> Key /= ?IF_NONE_MATCH end,Headers),
    [{?IF_NONE_MATCH,Etag}] ++ Filterd.

add_if_modified_since(undefined,Headers) -> Headers;
add_if_modified_since(Modified,Headers)->
    Filterd = lists:filter(fun({Key,_V}) -> Key /= ?IF_MODIFIED_SINCE end,Headers),
    [{?IF_MODIFIED_SINCE,Modified}] ++ Filterd.
add_content_encoding(undefined,Headers) -> Headers;
add_content_encoding(Encoding,Headers) when erlang:is_atom(Encoding)->
    add_content_encoding(erlang:atom_to_binary(Encoding,utf8),Headers);
add_content_encoding(Encoding,Headers)->
    Filterd = lists:filter(fun({Key,_V}) -> Key /= ?CONTENT_ENCODING end,Headers),
    [{?CONTENT_ENCODING,Encoding}] ++ Filterd.
add_content_length(undefined,Headers) -> Headers;
add_content_length(Length,Headers)->
    Filterd = lists:filter(fun({Key,_V}) -> Key /= ?CONTENT_LENGTH end,Headers),
    [{?CONTENT_LENGTH,erlang:integer_to_binary(Length)}] ++ Filterd.