%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(stetson).

-behaviour(application).

-include("include/stetson.hrl").

%% API
-export([start/0,
         stop/0,
         counter/2,
         counter/3,
         gauge/2,
         gauge/3,
         timer/2,
         timer/3]).

%% Callbacks
-export([start/2,
         stop/1]).

%%
%% API
%%

-spec start() -> ok.
%% @doc
start() -> application:start(?MODULE).

-spec stop() -> ok.
%% @doc
stop() -> application:stop(?MODULE).

-spec counter(atom(), integer()) -> ok.
%% @doc
counter(Stat, Step) -> stetson_server:cast({counter, Stat, Step}).

-spec counter(atom() | string(), integer(), float()) -> ok.
%% @doc
counter(Bucket, Step, Rate) -> stetson_server:cast({counter, Bucket, Step, Rate}).

-spec gauge(atom(), integer()) -> ok.
%% @doc
gauge(Stat, Step) -> stetson_server:cast({gauge, Stat, Step}).

-spec gauge(atom() | string(), integer(), float()) -> ok.
%% @doc
gauge(Bucket, Step, Rate) -> stetson_server:cast({gauge, Bucket, Step, Rate}).

-spec timer(atom() | string(), pos_integer()) -> ok.
%% @doc
timer(Bucket, Ms) -> stetson_server:cast({timer, Bucket, Ms}).

-spec timer(atom() | string(), pos_integer(), float()) -> ok.
%% @doc
timer(Bucket, Ms, Rate) -> stetson_server:cast({timer, Bucket, Ms, Rate}).

%%
%% Callbacks
%%

-spec start(normal, _Args) -> {ok, pid()} | {error, _}.
%% @hidden
start(normal, _Args) ->
    Uri = env(?STATSD_URI, "localhost:8126"),
    Ns  = env(?GRAPHITE_NS, ""),
    case stetson_sup:start_link(Uri, Ns) of
        ignore -> {error, sup_returned_ignore};
        Ret    -> Ret
    end.

-spec stop(_) -> ok.
%% @hidden
stop(_Args) -> ok.

%%
%% Private
%%

-spec env(atom(), any()) -> any().
%% @doc
env(Key, Default) ->
    application:load(?MODULE),
    case application:get_env(?MODULE, Key) of
        undefined   -> error({config_not_found, Key});
        {ok, Value} -> os(Value, Default)
    end.

-spec os(atom() | string(), any()) -> string().
%% @doc Try and retrieve an os ENV variable if the supplied key is
%% an atom, falling back to the supplied default if it has not been set.
os(Key, Default) when is_atom(Key) ->
    case os:getenv(atom_to_list(Key)) of
        false -> Default;
        Env   -> Env
    end;
%% Anything other than an atom as a key, is considered a valid value,
%% and is returned as-is.
os(Value, _Default) ->
    Value.
