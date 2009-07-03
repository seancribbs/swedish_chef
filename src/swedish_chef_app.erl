%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the swedish_chef application.

-module(swedish_chef_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for swedish_chef.
start(_Type, _StartArgs) ->
    swedish_chef_deps:ensure(),
    swedish_chef_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for swedish_chef.
stop(_State) ->
    ok.
