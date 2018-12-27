PROJECT = aihttp
PROJECT_DESCRIPTION = http lib for products from ailink.io
PROJECT_VERSION = 0.1.0

DEPS = cowlib ailib
dep_cowlib_commit = 2.7.0
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.1.7
include erlang.mk
