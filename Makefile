PROJECT = aihttp
PROJECT_DESCRIPTION = http lib for products from ailink.io
PROJECT_VERSION = 0.2.0

DEPS = cowlib gun cowboy ailib
dep_cowlib_commit = 2.7.0
dep_gun_commit = 1.3.0
dep_cowboy_commit = 2.6.1
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.3.2

include erlang.mk
