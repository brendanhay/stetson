%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(stetson_tests).

-include_lib("eunit/include/eunit.hrl").

-include("include/stetson.hrl").

-compile(export_all).

-define(HOST, "localhost").
-define(PORT, 8126).
-define(NS,  "ns").
-define(SOCK, socket).

%%
%% Unit
%%

setup() ->
    meck:new(gen_udp, [unstick, no_link]),
    meck:expect(gen_udp, open, 2, {ok, ?SOCK}),
    meck:expect(gen_udp, close, 1, ok),
    meck:expect(gen_udp, send, 4, ok),
    meck:new(timer, [unstick, passthrough, no_link]),
    meck:expect(timer, now_diff, 2, 15000),

    %% Start it up!
    Uri = lists:flatten(io_lib:format("~s:~B", [?HOST, ?PORT])),
    stetson_server:start(Uri, ?NS),

    %% Mods to unload
    [gen_udp, timer].

teardown(Mods) ->
    [?assert(meck:validate(M)) || M <- Mods],
    meck:unload(Mods).

stetson_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Registers named process",
       ?_assert(is_pid(whereis(?SERVER)))},

      {"Sends counter",
       ?_test(
          begin
              ?assertMatch(ok, stetson:counter("pos_counter", 1)),
              ?assert(send_called("pos_counter:1|c"))
          end)},

      {"Sends negative counter",
       ?_test(
          begin
              ?assertMatch(ok, stetson:counter("neg_counter", -5)),
              ?assert(send_called("neg_counter:-5|c"))
          end)},

      {"Sends gauge",
       ?_test(
          begin
              ?assertMatch(ok, stetson:gauge("pos_gauge", 1)),
              ?assert(send_called("pos_gauge:1|g"))
          end)},

      {"Sends negative gauge",
       ?_test(
          begin
              ?assertMatch(ok, stetson:gauge("neg_gauge", -5)),
              ?assert(send_called("neg_gauge:-5|g"))
          end)},

      {"Sends timer",
       ?_test(
          begin
              ?assertMatch(ok, stetson:timer("pos_timer", 1234)),
              ?assert(send_called("pos_timer:1234|ms"))
          end)},

      {"Sends negative timer",
       ?_test(
          begin
              ?assertMatch(ok, stetson:timer("neg_timer", -9999)),
              ?assert(send_called("neg_timer:-9999|ms"))
          end)}
     ]}.

%%
%% Helpers
%%

send_called(Msg) ->
    erlang:bump_reductions(2000), %% Force a context switch
    Expected = list_to_binary(string:join([?NS, Msg], ".")),
    case meck:called(gen_udp, send, [?SOCK, ?HOST, ?PORT, Expected]) of
        false -> error({not_sent, Expected, meck:history(gen_udp)});
        Other -> Other
    end.
