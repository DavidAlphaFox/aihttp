-record(ai_http_cache,{
	key :: binary(),
	cache_key :: term(),
	date :: binary(),
	etag :: binary(),
	strategy :: atom(), %% no_cache | age
	age :: integer(),
	last_modified :: binary(),
	headers :: term()
}).

-define(CONTENT_LENGTH,<<"content-length">>).
-define(ACCEPT_RANGES,<<"accept-ranges">>).
-define(CONTENT_RANGE,<<"content-range">>).
-define(RANGE,<<"range">>).
-define(CACHE_CONTROL,<<"cache-control">>).
-define(ETAG,<<"etag">>).
-define(LAST_MODIFIED,<<"last-modified">>).
-define(DATE,<<"date">>).
-define(CONTENT_ENCODING,<<"content-encoding">>).

-define(IF_NONE_MATCH,<<"if-none-match">>).
-define(IF_MODIFIED_SINCE,<<"if-modified-since">>).

-define(NO_STORE,<<"no-store">>).
-define(NO_CACHE,<<"no-cache">>).
-define(BYTES,<<"bytes">>).
-define(NONE,<<"none">>).
