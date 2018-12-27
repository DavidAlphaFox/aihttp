-module(ai_http_body).
-export([encode/2,decode/2]).

-spec decode(atom()|binary(),binary())-> binary() | {ok,binary()}.
decode(<<"gzip">>,Body)->{ok,zlib:gunzip(Body)};
decode(<<"deflate">>,Body) -> {ok,zlib:unzip(Body)};
decode(gzip,Body)->{ok,zlib:gunzip(Body)};
decode(deflate,Body) -> {ok,zlib:unzip(Body)};
decode(_,Body) -> {ok,Body}.
-spec encode(atom() | binary(),binary())-> binary() | {ok,binary()}.
encode(gzip,Body) -> {ok,zlib:gzip(Body)};
encode(deflate,Body) -> {ok,zlib:zip(Body)};
encode(<<"gzip">>,Body)->{ok,zlib:gzip(Body)};
encode(<<"deflate">>,Body) -> {ok,zlib:zip(Body)};
encode(_,Body) -> {ok,Body}.