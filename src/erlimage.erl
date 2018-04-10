-module(erlimage).
-export([
    load/1 
    ,x/0
    ,t/0
    ,z/1
    ,readFile/1
]).
-on_load(init/0).

-include("dev.hrl").

-define(APPNAME, erlimage).
-define(LIBNAME, erlimage_nif).

x() ->
    Resp = t(),
    {width, Width,_,_,_,_,_,_,_,_} = Resp,
    ?PRINT(Width), 
    z(Resp).

readFile (_) -> 
    not_loaded(?LINE).

load(_) ->
    not_loaded(?LINE).
t() ->
    not_loaded(?LINE).
z(_) ->
    not_loaded(?LINE).

init() ->
    PrivDir = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true -> filename:nativename( filename:absname( filename:join(["..", priv]) ) );
                _ -> filename:nativename( filename:absname( filename:join([priv]) ) )
            end;
        Dir -> filename:nativename( filename:absname( Dir ) )
    end,
    

    case os:type() of
        {win32, _Osname} -> 
            Path = os:getenv("PATH"),
            os:putenv("PATH", Path ++ PrivDir ++ ";");
        _True -> ok
    end,    

    SharedLib = filename:join(PrivDir, ?LIBNAME),     
    erlang:load_nif(SharedLib, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
