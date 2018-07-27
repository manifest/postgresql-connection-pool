# PostgreSQL connection pool

PostgreSQL connection pool library with support for re-establishing of lost connection.

[![Build Status][travis-img]][travis]



### How To Use

To build and start playing with the library, execute following commands in the shell:

```bash
make app shell
```

Here is a minimal example of using the PostgreSQL connection pool library.

```erlang
%% Creating a pool and adding it to the supervision tree.
Pool =
    #{name => default,
      size => 5,
      proc =>
        #{connection =>
            #{host => "localhost",
              username => "johndoe",
              database => "example"}}},
ChildSpec = pgsqlc_pool:child_spec(Pool),
supervisor:start_child(whereis(pgsqlc_pool_sup), ChildSpec).

%% Getting a connection and locking it to the current process.
%% If process dies, connection will be released.
Pid = pgsqlc_pool:lock(default),
%% Executing a query.
Ref = pgsqlc:query(Pid, "select 1;"),
%% Awaiting result.
pgsqlc:await(Pid, Ref),
%% Releasing the connection.
pgsqlc_pool:unlock(default, Pid).
```



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/postgresql-connection-pool?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/postgresql-connection-pool.png?branch=master
