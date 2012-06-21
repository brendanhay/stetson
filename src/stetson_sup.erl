%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(stetson_sup).

-behaviour(supervisor).

-include("include/stetson.hrl").

%% API
-export([start_link/2]).

%% Callbacks
-export([init/1]).

%%
%% API
%%

-spec start_link(string(), string()) -> {ok, pid()} | ignore | {error, _}.
%% @doc
start_link(Uri, Ns) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Uri, Ns}).

%%
%% Callbacks
%%

-spec init({string(), string()}) -> {ok, {{one_for_all, 3, 20}, [supervisor:child_spec()]}}.
%% @hidden
init(Config) ->
    Spec = {stats, {stetson_server, start_link, [Config]},
            permanent, 2000, worker, [stetson_server]},
    {ok, {{one_for_all, 3, 20}, [Spec]}}.
