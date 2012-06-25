%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(stetson_server).

-behaviour(gen_server).

-include("include/stetson.hrl").

%% API
-export([start/2,
         start_link/2,
         cast/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type message() :: {connect, pid(), inet:socket(), erlang:timestamp()} |
                   {establish, pid(), node()} |
                   {counter | gauge | timer, atom(), integer()} |
                   {counter | gauge | timer, atom(), integer(), float()}.

-export_type([message/0]).

-record(s, {sock               :: gen_udp:socket(),
            host = "localhost" :: string(),
            port = 8126        :: inet:port_number(),
            ns   = ""          :: string()}).

%%
%% API
%%

-spec start(string(), string()) -> ignore | {error, _} | {ok, pid()}.
%% @doc Start the stats process without a process link.
start(Uri, Ns) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Uri, Ns], []).

-spec start_link(string(), string()) -> ignore | {error, _} | {ok, pid()}.
%% @doc Start the stats process in the standard OTP fashion.
start_link(Uri, Ns) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Uri, Ns], []).

-spec cast(message()) -> ok.
cast(Msg) -> gen_server:cast(?SERVER, Msg).

%%
%% Callbacks
%%

-spec init([string(), ...]) -> {ok, #s{}}.
%% @hidden
init([Uri, Ns]) ->
    process_flag(trap_exit, true),
    _Ignore = random:seed(now()),
    {Host, Port} = split_uri(Uri, 8126),
    error_logger:info_msg("stetson will use statsd at ~s:~B~n", [Host, Port]),
    {ok, Sock} = gen_udp:open(0, [binary]),
    {ok, #s{sock = Sock, host = Host, port = Port, ns = Ns}}.

-spec handle_call(message(), _, #s{}) -> {reply, ok, #s{}}.
%% @hidden
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(message(), #s{}) -> {noreply, #s{}}.
%% @hidden Send counter/gauge/timer
handle_cast({Type, Bucket, N}, State) ->
    ok = stat(State, Type, Bucket, N),
    {noreply, State};
handle_cast({Type, Bucket, N, Rate}, State) ->
    ok = stat(State, Type, Bucket, N, Rate),
    {noreply, State}.

-spec handle_info(_Info, #s{}) -> {noreply, #s{}}.
%% @hidden
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(_, #s{}) -> ok.
%% @hidden
terminate(_Reason, #s{sock = Sock}) ->
    ok = gen_udp:close(Sock),
    ok.

-spec code_change(_, #s{}, _) -> {ok, #s{}}.
%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private
%%

-spec stat(#s{}, counter | gauge | timer, string() | atom(), integer(), float()) -> ok.
%% @private Create a statistic entry with a sample rate
stat(State, Type, Bucket, N, Rate) when Rate < 1.0 ->
    case {Type, random:uniform() =< Rate} of
        {counter, true} -> send(State, "~s:~p|c|@~p",  [Bucket, N, Rate]);
        {gauge, true}   -> send(State, "~s:~p|g|@~p",  [Bucket, N, Rate]);
        {timer, true}   -> send(State, "~s:~p|ms|@~p", [Bucket, N, Rate]);
        _Ignore         -> ok
    end.

-spec stat(#s{}, counter | gauge | timer, string() | atom(), integer()) -> ok.
%% @doc Create a statistic entry with no sample rate
stat(State, counter, Bucket, N)  -> send(State, "~s:~p|c", [Bucket, N]);
stat(State, gauge, Bucket, N)    -> send(State, "~s:~p|g", [Bucket, N]);
stat(State, timer, Bucket, N)    -> send(State, "~s:~p|ms", [Bucket, N]).

-spec send(#s{}, string(), [atom() | non_neg_integer()]) -> ok.
%% @private Send the formatted binary packet over the udp socket,
%% prepending the ns/namespace
send(#s{sock = Sock, host = Host, port = Port, ns = Ns}, Format, Args) ->
    Msg = format(Ns, Format, Args),
    case gen_udp:send(Sock, Host, Port, Msg) of
        _Any -> ok
    end.

-spec split_uri(string(), inet:port_number()) -> {nonempty_string(), inet:port_number()}.
%% @private
split_uri(Uri, Default) ->
    case string:tokens(Uri, ":") of
        [H|P] when length(P) > 0 -> {H, list_to_integer(lists:flatten(P))};
        [H|_]                    -> {H, Default}
    end.

-spec format(string(), string(), [any(), ...]) -> string().
%% @private iolist_to_bin is used here even though gen_...:send variants
%% accept deep iolists, since it's easier to debug on the wire.
format([], Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args));
format(Ns, Format, Args) ->
    iolist_to_binary(io_lib:format("~s." ++ Format, [Ns|Args])).


