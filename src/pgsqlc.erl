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

-module(pgsqlc).

%% API
-export([
    query/2,
    query/3,
    await/2,
    await/3
]).

%% =============================================================================
%% API
%% =============================================================================

-spec query(pgsqlc_pool_proc:proc(), iodata()) -> reference().
query(Proc, Sql) ->
    pgsqlc_pool_proc:query(Proc, Sql).

-spec query(pgsqlc_pool_proc:proc(), iodata(), list(epgsql:bind_param())) -> reference().
query(Proc, Sql, Params) ->
    pgsqlc_pool_proc:query(Proc, Sql, Params).

-spec await(pgsqlc_pool_proc:proc(), reference()) -> any().
await(Proc, Mref) ->
    pgsqlc_pool_proc:await(Proc, Mref).

-spec await(pgsqlc_pool_proc:proc(), reference(), timeout()) -> any().
await(Proc, Mref, Timeout) ->
    pgsqlc_pool_proc:await(Proc, Mref, Timeout).