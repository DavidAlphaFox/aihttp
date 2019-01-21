
-module(ai_multipart).
-export([encode/1,encode/2]).
%% @doc encode a list of parts a multipart form.
%% Parts can be under the form:
%%  - `{file, Path}' : to send a file
%%  - `{file, Path, ExtraHeaders}' : to send a file with extra headers
%%  - `{file, Path, Name, ExtraHeaders}': to send a file with DOM element name and extra headers
%%  - `{multipart_mixed, Name, Boundary}' to send a mixed multipart.
%%  - `{multipart_mixed_eof, Boundary}': to signal the end of the mixed
%%  multipart boundary.
%%  - `{Name, Data}': to send a custom content as a part
%%  - `{Name, Data, ExtraHeaders}': the same as above but with extra
%%  headers.
-spec encode(list()) -> {binary(),binary(), integer()}.
encode(Parts) ->
    encode(Parts, boundary()).

-spec encode(list(), binary()) -> {binary(),binary(), integer()}.
encode(Parts, Boundary) ->
    {Size, Acc} = 
        lists:foldl(fun
            ({file, Path}, {AccSize, AccBin}) ->
                {MpHeader, Len} = file_header({file, Path}, Boundary),
                AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    {ok, Bin} = file:read_file(Path),
                    PartBin = << MpHeader/binary, Bin/binary , "\r\n" >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>};
                ({file, Path, ExtraHeaders}, {AccSize, AccBin}) ->
                    {MpHeader, Len} = file_header({file, Path,
                                                      ExtraHeaders}, Boundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    {ok, Bin} = file:read_file(Path),
                    PartBin = << MpHeader/binary, Bin/binary, "\r\n"  >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>};
                ({file, Path, {Disposition, Params}, ExtraHeaders}, {
                                AccSize, AccBin}) ->
                    {MpHeader, Len} = file_header({file, Path, {Disposition, Params},
                                                      ExtraHeaders}, Boundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    {ok, Bin} = file:read_file(Path),
                    PartBin = << MpHeader/binary, Bin/binary, "\r\n"  >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>};
                ({file, Path, Name, ExtraHeaders}, {AccSize, AccBin}) ->
                    {MpHeader, Len} = file_header({file, Path, Name,
                                                      ExtraHeaders}, Boundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    {ok, Bin} = file:read_file(Path),
                    PartBin = << MpHeader/binary, Bin/binary, "\r\n"  >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>};
                ({multipart_mixed, Name, MixedBoundary}, {AccSize, AccBin}) ->
                    {MpHeader, _} = mixed_header(Name, MixedBoundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + 2,
                    {AccSize1, << AccBin/binary, MpHeader/binary, "\r\n" >>};
                ({multipart_mixed_eof, MixedBoundary}, {AccSize, AccBin}) ->
                    Eof = eof(MixedBoundary),
                    {AccSize + byte_size(Eof) + 2, <<AccBin/binary,
                                                     Eof/binary, "\r\n" >>};
                ({Name, Bin}, {AccSize, AccBin}) when is_binary(Bin) ->
                    Len = byte_size(Bin),
                    {MpHeader, Len} = data_header({Name, Len}, Boundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    PartBin = << MpHeader/binary, Bin/binary, "\r\n" >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>};
                ({Name, Bin, ExtraHeaders}, {AccSize, AccBin})
                        when is_binary(Bin) ->
                    Len = byte_size(Bin),
                    {MpHeader, Len} = data_header({Name, Len, ExtraHeaders},
                                                     Boundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    PartBin = << MpHeader/binary, Bin/binary, "\r\n" >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>};
                ({Name, Bin, Disposition, ExtraHeaders}, {AccSize, AccBin})
                        when is_binary(Bin) ->
                    Len = byte_size(Bin),
                    {MpHeader, Len} = data_header({Name, Len, Disposition,
                                                      ExtraHeaders}, Boundary),
                    AccSize1 = AccSize + byte_size(MpHeader) + Len + 2,
                    PartBin = << MpHeader/binary, Bin/binary, "\r\n" >>,
                    {AccSize1, << AccBin/binary, PartBin/binary >>}
        end, {0, <<>>}, Parts),
    MpEof = eof(Boundary),
    FinalSize = Size + byte_size(MpEof),
    {Boundary,<< Acc/binary, MpEof/binary >>, FinalSize}.

-spec eof(Boundary:: binary()) -> binary().
eof(Boundary) ->
    <<"--",  Boundary/binary, "--\r\n">>.

-spec boundary() -> binary().
boundary() ->
    Unique = unique(16),
    <<"---------------------------", Unique/binary>>.

unique(Size) -> unique(Size, <<>>).
unique(0, Acc) -> Acc;
unique(Size, Acc) ->
  Random = $a + rand:uniform($z - $a),
  unique(Size - 1, <<Acc/binary, Random>>).

header(Headers, Boundary) ->
    BinHeaders = to_binary(Headers),
    <<"--", Boundary/binary, "\r\n", BinHeaders/binary >>.

to_binary(Headers) when is_list(Headers) ->
  HeadersList = 
    lists:foldl(fun
        ({Name, Value}, Acc) -> [make_header(Name, Value) | Acc];
        ({Name, Value, Params}, Acc) ->[make_header(Name, Value, Params) | Acc]
    end, [], Headers),
  iolist_to_binary([
    ai_string:join(lists:reverse(HeadersList), <<"\r\n">>),
    <<"\r\n\r\n">>]);
to_binary(Headers) -> to_binary(maps:to_list(Headers)).

%% @doc Create a binary header
make_header(Name, Value) ->
  Value1 = ai_string:to_string(Value),
  << Name/binary, ": ", Value1/binary >>.

make_header(Name, Value, Params) ->
  Value1 = header_value(ai_string:to_string(Value), Params),
  << Name/binary, ": ", Value1/binary >>.

%% @doc join value and params in a binary
header_value(Value, Params) when is_list(Value) ->
  header_value(list_to_binary(Value), Params);
header_value(Value, Params) ->
  Params1 = 
    lists:foldl(fun({K, V}, Acc) ->
        K1 = ai_string:to_string(K),
        V1 = ai_string:to_string(V),
        ParamStr = << K1/binary, "=", V1/binary  >>,
        [ParamStr | Acc]
    end, [], Params),
    ai_string:join([Value] ++ lists:reverse(Params1), "; ").

file_header({file, Path}, Boundary) ->
    file_header({file, Path, []}, Boundary);
file_header({file, Path, ExtraHeaders}, Boundary) ->
    file_header({file, Path, <<"file">>, ExtraHeaders}, Boundary);
file_header({file, Path, Name, ExtraHeaders}, Boundary) when is_binary(Name) ->
    FName = ai_string:to_string(filename:basename(Path)),
    Disposition = <<"form-data">>,
    Params = [
        {<<"name">>,     <<"\"", Name/binary,  "\"">>},
        {<<"filename">>, <<"\"", FName/binary, "\"">>}
    ],
    file_header({file, Path, {Disposition, Params}, ExtraHeaders}, Boundary);
file_header({file, Path, {Disposition, Params}, ExtraHeaders}, Boundary) ->
    CType = cow_mimetypes:all(Path),
    Len = filelib:file_size(Path),
    ExtraHeaders0 = lists:map(fun ({K, V}) -> {ai_string:to_lower(K), V} end, ExtraHeaders),
    Headers = filter_header([{<<"content-type">>, mimetype(CType)},
                                {<<"content-length">>, Len}],
                               [{<<"content-disposition">>, Disposition, Params} | ExtraHeaders0]),
    BinHeader = header(Headers, Boundary),
    {BinHeader, Len}.
%% @doc return the mixed multipart header
-spec mixed_header({Name :: binary(), MixedBoundary :: binary()}, Boundary :: binary())  ->
    {binary(), 0}.
mixed_header({Name, MixedBoundary}, Boundary) ->
    Headers = [{<<"Content-Disposition">>, <<"form-data">>,
                [{<<"name">>, <<"\"", Name/binary, "\"">>}]},
               {<<"Content-Type">>, <<"multipart/mixed">>,
                [{<<"boundary">>, MixedBoundary}]}],
    {header(Headers, Boundary), 0}.


%% @doc return the multipart header for a data
-spec data_header({Name:: binary(), DataLen :: integer()} |
                     {Name:: binary(), DataLen :: integer(),
                      ExtraHeaders ::[{binary(), binary()}]} |
                     {Name:: binary(), DataLen :: integer(),
                            {Disposition :: binary(), Params :: [{binary(), binary()}]},
                            ExtraHeaders :: [{binary(), binary()}]},
                     Boundary :: binary()) ->
    {binary(), DataLen :: integer()}.
data_header({Name, Len}, Boundary) ->
    data_header({Name, Len, []}, Boundary);
data_header({Name, Len, ExtraHeaders}, Boundary) ->
    Disposition = {<<"form-data">>, [{<<"name">>,<<"\"", Name/binary, "\"">>}]},
    data_header({Name, Len, Disposition, ExtraHeaders}, Boundary);
data_header({Name, Len, {Disposition, Params}, ExtraHeaders}, Boundary) ->
    CType = cow_mimetypes:all(Name),
    ExtraHeaders0 = lists:map(fun ({K, V}) -> {ai_string:to_lower(K), V} end, ExtraHeaders),
    Headers = filter_header([{<<"content-type">>, mimetype(CType)},
                                {<<"content-length">>, Len}],
                               [{<<"content-disposition">>, Disposition, Params} | ExtraHeaders0]),
    BinHeader = header(Headers, Boundary),
    {BinHeader, Len}.

filter_header([], Acc) -> Acc;
filter_header([{Key, Value} | Headers], Acc) ->
    case proplists:get_value(Key, Acc) of
        undefined ->
            filter_header(Headers, [{Key, Value} | Acc]);
        _Else ->
            filter_header(Headers, Acc)
    end.
mimetype({C, T, _Any})-> <<C/binary,"/",T/binary>>.