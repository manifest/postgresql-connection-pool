%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2018 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(pgsqlc_pool_proc).
-behaviour(poolboy_worker).

%% API
-export([
    query/2,
    query/3,
    await/2,
    await/3
]).

%% Poolboy callbacks
-export([
    start_link/1
]).

%% System callbacks
-export([
    system_continue/3,
    system_terminate/4,
    system_code_change/4,
    system_get_state/1,
    system_replace_state/2
]).

%% Loop
-export([
    init/2,
    loop/1
]).

%% Types
-record(state, {
    opts       :: map(),
    conn_opts  :: map(),
    conn       :: pid() | undefined,
    parent     :: pid(),
    last_error :: any()
}).

-type state() :: #state{}.
-type misc() :: {loop | retry_loop, list()}.
-type from() :: {pid(), reference()}.
-type proc() :: pid() | atom().
-type retry() :: non_neg_integer().

-export_types([from/0, proc/0, retry/0]).

%% Definitions
-define(DEFAULT_QUERY_TIMEOUT, 5000).
-define(DEFAULT_RETRY_TIMEOUT, 5000).
-define(DEFAULT_RETRY, 5).

%% =============================================================================
%% API
%% =============================================================================

-spec query(proc(), iodata()) -> reference().
query(Proc, Sql) ->
    query(Proc, Sql, []).

-spec query(proc(), iodata(), list(epgsql:bind_param())) -> reference().
query(Proc, Sql, Params) ->
    Mref = monitor(process, Proc),
    Req = {pgsqlc_query, {self(), Mref}, Sql, Params},
    catch erlang:send(Proc, Req),
    Mref.

-spec await(proc(), reference()) -> any().
await(Proc, Mref) ->
    await(Proc, Mref, ?DEFAULT_QUERY_TIMEOUT).

-spec await(proc(), reference(), timeout()) -> any().
await(Proc, Mref, Timeout) ->
    receive
        {Mref, Resp} ->
            demonitor(Mref, [flush]),
            Resp;
        {'DOWN', Mref, process, _, noconnection} ->
            exit({nodedown, node(Proc)});
        {'DOWN', Mref, process, _, Reason} ->
            exit(Reason)
    after Timeout ->
        demonitor(Mref, [flush]),
        exit(timeout)
    end.

%% =============================================================================
%% Poolboy callbacks
%% =============================================================================

-spec start_link(map()) -> {ok, pid()}.
start_link(Conf) ->
    proc_lib:start_link(?MODULE, init, [self(), Conf]).

%% =============================================================================
%% System callbacks
%% =============================================================================

-spec system_continue(pid(), list(), misc()) -> no_return().
system_continue(_Parent, _Debug, {Loop, Args}) ->
    erlang:apply(?MODULE, Loop, Args).

-spec system_terminate(any(), pid(), list(), misc()) -> no_return().
system_terminate(Reason, _Parent, _Debug, {_Loop, Args}) ->
    terminate(Reason, lists:last(Args)).

-spec system_code_change(misc(), module(), any(), any()) -> {ok, state()}.
system_code_change({_Loop, Args}, _Module, _OldVsn, _Extra) ->
    {ok, lists:last(Args)}.

-spec system_get_state(misc()) -> {ok, state()}.
system_get_state({_Loop, Args}) ->
    {ok, lists:last(Args)}.

-spec system_replace_state(fun((state()) -> state()), misc()) -> {ok, state(), misc()}.
system_replace_state(Fun, {_Loop, Args} = Misc) ->
    {ok, Fun(lists:last(Args)), Misc}.

%% =============================================================================
%% Loop
%% =============================================================================

-spec init(pid(), map()) -> no_return().
init(Parent, Conf) ->
    try init_state(Parent, Conf) of
        State ->
            proc_lib:init_ack(Parent, {ok, self()}),

            process_flag(trap_exit, true),
            connect(maps:get(retry, State#state.opts, ?DEFAULT_RETRY), State)
    catch
        _:Reason ->
            proc_lib:init_ack(Parent, {error, Reason})
    end.

-spec init_state(pid(), map()) -> state().
init_state(Parent, Conf) ->
    Opts = validate_options(Conf, #{}),
    ConnOpts = validate_connection(Conf),
    #state{
        opts = Opts,
        conn_opts = ConnOpts,
        parent = Parent}.

-spec connect(retry(), state()) -> no_return().
connect(Retry, #state{conn_opts = ConnOpts} = State) ->
    case epgsql:connect(ConnOpts) of
        {ok, Conn}      -> up(State#state{conn = Conn});
        {error, Reason} -> retry(Retry, State#state{last_error = Reason})
    end.

-spec up(state()) -> no_return().
up(#state{parent = Parent} =State) ->
    Parent ! {pgsqlc_up, self()},
    loop(State).

-spec down(any(), state()) -> no_return().
down(Reason, #state{parent = Parent, opts = Opts} =State) ->
    Parent ! {pgsqlc_down, self(), Reason},
    retry(maps:get(retry, Opts, ?DEFAULT_RETRY), State#state{last_error = Reason}).

-spec retry(retry(), state()) -> no_return().
retry(0, #state{last_error = Reason} =State) ->
    terminate({shutdown, Reason}, State);
retry(Retry, State) ->
    retry_loop(Retry -1, State).

-spec retry_loop(retry(), state()) -> no_return().
retry_loop(Retry, #state{parent = Parent, opts = Opts} =State) ->
    RetryTimeout = maps:get(retry_timeout, Opts, ?DEFAULT_RETRY_TIMEOUT),
    _ = erlang:send_after(RetryTimeout, self(), retry),
    receive
        retry ->
            connect(Retry, State);
        {system, From, Req} ->
            sys:handle_system_msg(
                Req, From, Parent, ?MODULE, [],
                {retry_loop, [Retry, State]})
    end.

-spec loop(state()) -> no_return().
loop(#state{parent = Parent, conn = Conn} = State) ->
    receive
        {pgsqlc_query, From, Sql, Params} ->
            handle_query(Conn, From, Sql, Params),
            loop(State);
        {'EXIT', Parent, Reason} ->
            terminate(Reason, State);
        {'EXIT', Conn, Reason} ->
            epgsql:close(Conn),
            down(Reason, State);
        {system, From, Req} ->
            sys:handle_system_msg(
                Req, From, Parent, ?MODULE, [],
                {loop, [State]});
        Req ->
            error_logger:info_msg("Unexpected message: message=~p", [Req]),
            loop(State)
    end.

-spec terminate(any(), state()) -> no_return().
terminate(Reason, #state{conn = Conn}) ->
    epgsql:close(Conn),
    exit(Reason).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec handle_query(pid(), from(), iodata(), epgsql:bind_param()) -> ok.
handle_query(Conn, {To, Mref}, Sql, Params) ->
    Result = epgsql:equery(Conn, Sql, Params),
    To ! {Mref, Result},
    ok.

-spec validate_options(map(), map()) -> map().
validate_options(#{options := Val}, _Default) when is_map(Val) ->
    Val;
validate_options(#{options := Val}, _Default) ->
    error({invalid_options, Val});
validate_options(_, Default) ->
    Default.

-spec validate_connection(map()) -> map().
validate_connection(#{connection := Val}) ->
    try
        true = is_map(Val),
        _ = validate_host(Val),
        _ = validate_username(Val),
        Val
    catch _:_ ->
        error({invalid_connection, Val})
    end;
validate_connection(_) ->
    error(missing_connection).

-spec validate_host(map()) -> list().
validate_host(#{host := Val}) when is_list(Val) -> Val;
validate_host(#{host := Val})                   -> throw({invalid_host, Val});
validate_host(_)                                -> throw(missing_host).

-spec validate_username(map()) -> list().
validate_username(#{username := Val}) when is_list(Val) -> Val;
validate_username(#{username := Val})                   -> throw({invalid_username, Val});
validate_username(_)                                    -> throw(missing_username).