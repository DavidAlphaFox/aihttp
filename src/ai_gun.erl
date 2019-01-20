-module(ai_gun).
-export([method/1,urlencode_pairs/1]).


urlencode_pairs(Tokens) when is_map(Tokens) ->
    urlencode_pairs(maps:to_list(Tokens));
urlencode_pairs(Tokens) ->
    Q = lists:map(fun({Key,Value})->
            EKey = cow_qs:urlencode(ai_string:to_string(Key)),
            EValue = cow_qs:urlencode(ai_string:to_string(Value)),
            <<EKey/binary,"=",EValue/binary>>
        end,Tokens),
    ai_string:join(Q,<<"&">>).

method(Method) when is_binary(Method) -> 
    string:to_upper(binary_to_list(Method));
method(Method) when is_list(Method) ->
    string:to_upper(Method);
method(put) ->"PUT";
method(delete) ->"DELETE";
method(patch) ->"PATCH";
method(options) ->"OPTIONS";
method(get) ->"GET";
method(post) ->"POST";
method(head) ->"HEAD".


