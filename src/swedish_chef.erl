%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(swedish_chef).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the swedish_chef server.
start() ->
    swedish_chef_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(swedish_chef).

%% @spec stop() -> ok
%% @doc Stop the swedish_chef server.
stop() ->
    Res = application:stop(swedish_chef),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
