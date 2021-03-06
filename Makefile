PROJECT = pgsqlc_pool
PROJECT_DESCRIPTION = PostgreSQL connection pool

DEPS = \
	poolboy \
	epgsql

BUILD_DEPS = \
	version.mk

DEP_PLUGINS = \
	version.mk

dep_poolboy = git https://github.com/manifest/poolboy.git feature/worker-args-any
dep_epgsql = git https://github.com/epgsql/epgsql.git 4.1.0
dep_version.mk = git https://github.com/manifest/version.mk.git v0.2.0

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start

include erlang.mk

app:: rebar.config

.PHONY: elvis
elvis:
	./elvis rock -c elvis.config
