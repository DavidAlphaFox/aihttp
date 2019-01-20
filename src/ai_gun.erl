-module(ai_gun).
-export([method/1]).

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