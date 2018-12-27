-module(ai_url).

-include("ai_url.hrl").

-export([parse/1]).
-export([build/1]).

%% This is from chapter 3, Syntax Components, of RFC 3986: 
%% 
%% The generic URI syntax consists of a hierarchical sequence of
%% components referred to as the scheme, authority, path, query, and
%% fragment.
%% 
%%    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%% 
%%    hier-part   = "//" authority path-abempty
%%                   / path-absolute
%%                   / path-rootless
%%                   / path-empty
%% 
%%    The scheme and path components are required, though the path may be
%%    empty (no characters).  When authority is present, the path must
%%    either be empty or begin with a slash ("/") character.  When
%%    authority is not present, the path cannot begin with two slash
%%    characters ("//").  These restrictions result in five different ABNF
%%    rules for a path (Section 3.3), only one of which will match any
%%    given URI reference.
%% 
%%    The following are two example URIs and their component parts:
%% 
%%          foo://example.com:8042/over/there?name=ferret#nose
%%          \_/   \______________/\_________/ \_________/ \__/
%%           |           |            |            |        |
%%        scheme     authority       path        query   fragment
%%           |   _____________________|__
%%          / \ /                        \
%%          urn:example:animal:ferret:nose
%% 
%%    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
%%    authority   = [ userinfo "@" ] host [ ":" port ]
%%    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

parse(U)->
    UBinary = ai_string:to_string(U),
    parse(schema,UBinary,#ai_url{}).
parse(schema,Bin,Acc)->
    case binary:match(Bin, [<<":">>]) of 
        nomatch -> 
            parse(authority,Bin,Acc);
        {S,L}->
            %% maybe this example.com:3333/over/there?name=ferret#nose
            MaybeSchema = binary:part(Bin, 0, S),
            Pos = S + L, %%binary的切开点，Pos是未匹配字串的第一个字符
            DoubleSlash = binary:at(Bin,Pos) =:= $/ andalso binary:at(Bin,Pos + 1) =:= $/,
            if 
                DoubleSlash == true ->
                    Rest  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
                    parse(authority,Rest,Acc#ai_url{schema = MaybeSchema});
                true ->
                    parse(authority,Bin,Acc)
            end
    end;
parse(authority,<<"//",Bin/bits>>,Acc)->
    case binary:match(Bin, [<<":">>]) of 
        nomatch -> 
            %% example.com/a
            %% example.com/
            %% example.com
            case binary:match(Bin,[<<"/">>]) of 
                nomatch -> Acc#ai_url{authority = Bin,host = Bin,path = <<"/">>}; %example.com
                {S1,_L1}->
                    Authority = binary:part(Bin,0,S1),
                    Rest = binary:part(Bin,S1,byte_size(Bin) - S1),
                    parse(path,Rest,Acc#ai_url{authority = Authority,host = Bin})
            end;
        {S,L}->
            case binary:match(Bin,[<<"/">>]) of 
                nomatch -> 
                    Pos = S + L,
                    Port = binary:part(Bin,Pos,byte_size(Bin) - Pos),
                    Host = binary:part(Bin,0,S),
                    Acc#ai_url{authority = Bin,host = Host,port = Port,path = <<"/">>}; %example.com
                {S2,_L2}->
                    Pos = S + L,
                    Port = binary:part(Bin,Pos,S2 - Pos),
                    Host = binary:part(Bin,0,S),
                    Rest = binary:part(Bin,S2,byte_size(Bin) - S2),
                    parse(path,Rest,Acc#ai_url{authority = Bin,host = Host,port = Port})
                    
            end
    end;
%% no schema here, but can have authority
parse(authority,Bin,Acc)-> 
   case binary:match(Bin,[<<".">>]) of 
        nomatch -> parse(path,Bin,Acc);
        _ -> parse(authority,<<"//",Bin/binary>>,Acc)
    end;    

parse(path,Bin,Acc)->
    case binary:match(Bin,[<<"?">>]) of 
        nomatch ->
            %% example.com/a/b/c
            %% example.com/a/b/c#c=x&d=n
            case binary:match(Bin,[<<"#">>]) of 
                nomatch -> %% example.com/a/b/c
                    Acc#ai_url{path = Bin};
                {S,L}->
                    Path = binary:part(Bin,0,S),
                    Pos = S + L,
                    Rest  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
                    parse(fragment,Rest,Acc#ai_url{path = Path})
            end;
        {S1,L1}->
            %% example.com/a/b/c?1=&2=
            %% example.com/a/b/c?1=&2=#c=x&d=n
            Path = binary:part(Bin,0,S1),
            Pos = S1 + L1,
            Rest = binary:part(Bin,Pos,byte_size(Bin) - Pos),
            parse(qs,Rest,Acc#ai_url{path = Path})
    end;
parse(qs,Bin,Acc)->
    case binary:match(Bin,[<<"#">>]) of 
        nomatch -> 
            QS = cow_qs:parse_qs(Bin),
            Acc#ai_url{qs = QS};
        {S,L} ->
            Query = binary:part(Bin,0,S),
            QS = cow_qs:parse_qs(Query),
            Pos = S + L,
            Rest = binary:part(Bin,Pos,byte_size(Bin) - Pos),
            parse(fragment,Rest,Acc#ai_url{qs = QS})
end;
parse(fragment,Bin,Acc)->
    QS = cow_qs:parse_qs(Bin),
    Acc#ai_url{fragment = QS}.


build(Record)->
    build(schema,Record,<<>>).
build(schema,Record,Acc)->
    case Record#ai_url.schema of 
        undefined ->
            build(authority,Record,Acc);
        Schema ->
            SchemaBin = ai_string:to_string(Schema),
            build(authority,Record,<<Acc/binary,SchemaBin/binary,"://">>)
    end;
build(authority,Record,Acc)->
    case {Record#ai_url.authority,Record#ai_url.host} of 
        {undefined,undefined} ->  build(path,Record,Acc);
        {undefined,Host}->
            case Record#ai_url.port of 
                undefined ->
                    HostBin = ai_string:to_string(Host),
                    build(path,Record,<<Acc/binary,HostBin/binary>>);
                Port -> 
                    HostBin = ai_string:to_string(Host),
                    PortBin = ai_string:to_string(Port),
                    build(path,Record,<<Acc/binary,HostBin/binary,":",PortBin/binary>>)
            end;
        {Authority,_Host}-> 
            AuthorityBin = ai_string:to_string(Authority),
            build(path,Record,<<Acc/binary,AuthorityBin/binary>>)
    end;
build(path,Record,Acc)->
    case Record#ai_url.path of 
        undefined -> build(qs,Record,<<Acc/binary,"/">>);
        Path ->
            PathBin = ai_string:to_string(Path),
            build(qs,Record,<<Acc/binary,PathBin/binary>>)
    end;
build(qs,Record,Acc)->
    case Record#ai_url.qs of 
        undefined -> build(fragment,Record,Acc);
        QS ->
            Q = lists:map(fun({Key,Value})->
                    EKey = cow_qs:urlencode(ai_string:to_string(Key)),
                    EValue = cow_qs:urlencode(ai_string:to_string(Value)),
                    <<EKey/binary,"=",EValue/binary>>
                    end,QS),
            S = lists:foldl(
                    fun
                        (I,QAcc) when erlang:is_atom(QAcc)-> <<"?",I/binary>> ;
                        (I,QAcc) ->  <<QAcc/binary,"&",I/binary>>  
                    end, undefined,Q),
            build(fragment,Record,<<Acc/binary,S/binary>>)
    end;
build(fragment,Record,Acc)->
    case Record#ai_url.fragment of 
        undefined -> Acc;
        QS ->
            Q = lists:map(fun({Key,Value})->
                    EKey = cow_qs:urlencode(ai_string:to_string(Key)),
                    EValue = cow_qs:urlencode(ai_string:to_string(Value)),
                    <<EKey/binary,"=",EValue/binary>>
                    end,QS),
            S = lists:foldl(
                    fun
                        (I,QAcc) when erlang:is_atom(QAcc)-> <<"#",I/binary>> ;
                        (I,QAcc) ->  <<QAcc/binary,"&",I/binary>>  
                    end, undefined,Q),
            <<Acc/binary,S/binary>>
    end.
                    