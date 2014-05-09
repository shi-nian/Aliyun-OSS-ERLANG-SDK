-module(alioss).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-define(KEY, "your-key").
-define(SECRET, "your-secret").
%-define(HOST, "oss-cn-hangzhou.aliyuncs.com").
-define(HOST, "oss.aliyuncs.com").
%-define(HOST, "www.test.local").

-define(HTTPC_OPTION, [{sync, false},{headers_as_is, true}]).

get_header() ->
    get_header("GET", "/", "", ?HOST, "").

get_header(M, R) ->
    get_header(M, R, "", ?HOST, "").

%get_header("GET", "/", Content_Type, Host, "0"),
get_header(Method, Resource, Content_Type, Host, Content_Length) ->
    CL = case Content_Length =:= "" of
        true -> "0";
        false -> be_list(Content_Length)
    end,
    Date = format_utc_timestamp(),
    Authorization = "OSS " ++ ?KEY ++ ":" ++ signature(Method, Date, Resource, Content_Type),
    A = io_lib:format("~s",[Authorization]),
    %[{"Date", Date},{"Host",?HOST}, {"Authorization", A}].
    Basic = [{"Date", Date}, {"Authorization", A}, {"Host", Host}, {"Content-Length", CL}],
    case Content_Type =:= "" of
        true -> Basic;
        false -> [{"Content-Type", Content_Type} | Basic]
    end.

signature() ->
    signature("GET", format_utc_timestamp()).

signature(Method, Date) ->
    signature(Method, Date, "/", "").

signature(Method, Date, Resource, C) ->
    Content_Type = C ++ "\n",
    %Data = "GET\n\n\nThu, 08 May 2014 07:34:44 GMT\n/",
    Data = Method ++ "\n\n"++ Content_Type ++ Date ++ "\n" ++ Resource,
    erlang:display([?LINE, "signdata", Data]),
    base64:encode(crypto:sha_mac(?SECRET, Data)).


object_delete() ->
    inets:start(),
    Source = "shopex-ecae-test",
    Object_key = "test/first.html",
    URL = "http://"  ++ Source ++ "." ++ ?HOST ++ "/" ++ Object_key,
    Content_Type = "",
    Host = Source++"."++?HOST,
    Headers = get_header("DELETE", "/"++Source++"/"++Object_key, Content_Type, Host, ""),
    {ok, Request_id} = httpc:request(delete, 
        {URL, Headers},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            erlang:display(List),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.


object_get() ->
    inets:start(),
    Source = "shopex-ecae-test",
    Object_key = "test/first.html",
    URL = "http://"  ++ Source ++ "." ++ ?HOST ++ "/" ++ Object_key,
    Content_Type = "",
    Host = Source++"."++?HOST,
    Headers = get_header("GET", "/"++Source++"/"++Object_key, Content_Type, Host, ""),
    {ok, Request_id} = httpc:request(get, 
        {URL, Headers},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            erlang:display(List),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.


object_put() ->
    inets:start(),
    Source = "shopex-ecae-test",
    Object_key = "test/first.html",
    URL = "http://"  ++ Source ++ "." ++ ?HOST ++ "/" ++ Object_key,
    Content_Type = "text/html",
    Body = "test put",
    Host = Source++"."++?HOST,
    Headers = get_header("PUT", "/"++Source++"/"++Object_key, Content_Type, Host, erlang:length(Body)),
    {ok, Request_id} = httpc:request(put, 
        {URL, Headers, Content_Type, Body},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            erlang:display(List),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.



bucket_delete() ->
    inets:start(),
    Source = "shopex-ecae-test",
    URL = "http://"  ++ Source ++ "." ++ ?HOST,
    Content_Type = "",
    Host = Source++"."++?HOST,
    Headers = get_header("DELETE", "/"++Source++"/", Content_Type, Host, ""),
    {ok, Request_id} = httpc:request(delete, 
        {URL, Headers},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            erlang:display(List),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.



bucket_getacl() ->
    inets:start(),
    Source = "shopex-ecae-test",
    URL = "http://"  ++ Source ++ "." ++ ?HOST ++ "/?acl",

    Content_Type = "",
    Host = Source++"."++?HOST,
    Headers = get_header("GET", "/"++Source++"/?acl", Content_Type, Host, ""),

    {ok, Request_id} = httpc:request(get, 
        {URL, Headers},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            {Doc, _} = xmerl_scan:string(List),
            AtomList = xmerl_xpath:string("/AccessControlPolicy/AccessControlList/Grant", Doc),
            parse_xml(AtomList),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.

bucket_create() ->
    inets:start(),
    Source = "shopex-ecae-test",
    URL = "http://"  ++ Source ++ "." ++ ?HOST,

    Content_Type = "",
    Host = Source++"."++?HOST,
    Headers = get_header("PUT", "/"++Source++"/", Content_Type, Host, ""),

    {ok, Request_id} = httpc:request(put, 
        {URL, Headers, "", ""},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            erlang:display(List),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.


bucket_list() ->
    inets:start(),

    Content_Type = "",
    Host = ?HOST,
    Headers = get_header("GET", "/", Content_Type, Host, ""),

    %Content-Length
    URL = "http://" ++ ?HOST,
    {ok, Request_id} = httpc:request(get, 
        {URL, Headers},
        [], ?HTTPC_OPTION),
    erlang:display(Request_id),
    receive
        {http, {Request_id, {{_, 200, _}, _, Output}}} ->
            List = binary_to_list(Output),
            {Doc, _} = xmerl_scan:string(List),
            AtomList = xmerl_xpath:string("/ListAllMyBucketsResult/Buckets/*/Name", Doc),
            parse_xml(AtomList),
            ok;
        Out -> io:format("~p ~p ~n", [?LINE, Out])
    after 10000 ->
            io:format("[~p, ~p],get site list failed~n", [?LINE, ?MODULE])
    end,
    ok.


parse_xml([]) ->
    ok;
parse_xml([XmlElement | X]) ->
%    erlang:display([?LINE, XmlElement]),
%    erlang:display([?LINE, X]),
    case is_record(XmlElement, xmlElement) of
        true -> 
            XmlText = hd(XmlElement#xmlElement.content),
%            erlang:display(XmlText),
            io:format("~p ~n", [XmlText#xmlText.value]),
            parse_xml(X);
        false -> parse_xml(X)
    end.



format_utc_timestamp() ->  
    TS = {_,_,Micro} = os:timestamp(),  
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),  
    Week = element(calendar:day_of_the_week(Year, Month, Day), {"Mon","Tue","Wed","Thu","Fri","Sat","Sun"}),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),  
    R = lists:flatten(io_lib:format("~s, ~2..0w ~s ~4w ~2..0w:~2..0w:~2..0w GMT",  [Week,Day,Mstr,Year,Hour,Minute,Second])),
    R.


be_list(X) when is_list(X)-> X;
be_list(X) when is_integer(X) -> integer_to_list(X);
be_list(X) when is_atom(X) -> atom_to_list(X);
be_list(X) when is_binary(X) -> binary_to_list(X);
be_list(X) -> lists:flatten(io_lib:format("~p",[X])).



