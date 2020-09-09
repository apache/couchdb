# Description

The `couch_rate` application implements a generic rate limiter which can
be used to control batch size and delay between batches. It was initially
designed for background index build to find an optimal batch size to utilize
the FDB transaction up to configured `target` parameter. The application
provides an API to plug custom rate limiting logic when need to.

# Default rate limit logic

The `couch_rate_limiter` is the rate limit module used by default.
The module tracks average number of reads and writes over specified
time period. It uses average read/write numbers to calculate an
approximate value for read/write ratio. Then the read/write ratio is
used to convert estimated amount of writes into batch size.

# Configuration

## API based usage

In the simplest use case the only mandatory keys `new/3` expects are:
* `budget` - the initial value for estimated batch size
* `target` - the amount in msec which we try to maintain for batch processing time
* `window` - time interval for contention detector
* `sensitivity` - minimal interval within the `window`

We choose sane default values for the rest of the parameters.

* `window_size = window div sensitivity + 1`
* `underload_threshold = round(target * 0.95)`
* `overload_threshold = round(target * 1.05)`
* `delay_threshold = round(target * 1.07)`

Due to the use of `round` in defaults calculation the `target` cannot be less
than `36` msec. Otherwise some of the thresholds become equal which breaks the
algorithm.

In the case when you need to specify custom parameters, the following keys
are supported:

* `window_size` - how many batches to consider in contention detector
* `timer` - this is used for testing to fast forward time `fun() -> current_time_in_ms() end`
* `target` - the amount in msec which we try to maintain for batch processing time
* `underload_threshold` - a threshold bellow which we would try to increase the budget
* `overload_threshold` - a threshold above which we would start decreasing the budget
* `delay_threshold` - a threshold above which we would start introducing delays between batches
* `multiplicative_factor` - determines how fast we are going to decrease budget (must be in (0..1) range)
* `regular_delay` - delay between batches when there is no overload
* `congested_delay` - delay between batches when there is an overload
* `initial_budget` - initial value for budget to start with

## default.ini based usage

The users of the `couch_rate` application pass the `ConfigId` parameter.
When calling `couch_rate:new` and `couch_rate:create_if_missing`.
The `couch_rate` application uses this information to construct name of the
configuration section to use to get configuration parameters. The configration
section is constructed using `"couch_rate." ++ ConfigId`.
The parameters are encoded using erlang map syntax.
Limitation of the map parser:

* Keys must be atoms
* Values are either integers or floats
* We only support positive values in the map
* Configuration object cannot use erlang reserved words in keys:
  `after`, `and`, `andalso`, `band`, `begin`, `bnot`, `bor`,
  `bsl`, `bsr`, `bxor`, `case`, `catch`, `cond`, `div`, `end`
  `fun`, `if`, `let`, `not`, `of`, `or`, `orelse`, `receive`
  `rem`, `try`, `when`, `xor`

The auxilary `couch_rate_config` module implements the following API:

* `couch_rate_config:from_str/1` - parses a string representation of parameters
* `couch_rate_config:to_str/1` - converts parameters to string (used in testing)

Here is the example of configuration used in `couch_view` application:

```
[couch_rate.views]
limiter = couch_rate_limiter
opts = #{budget => 100, target => 2500, window => 60000, sensitivity => 1000}
```

In the `couch_view` application it is used as follows:

```
Limiter = couch_rate:create_if_missing({DbName, DDocId}, "views"),
```

# API

The application implements two APIs. Both APIs are supported by `couch_rate`
module. The API variants are:

* explicit state passing
* state store based approach

The API is chosen baed on the `StoreModule` argument passed to `new/4`.
Currently we support following values for `StoreModule`:

* `nil` - this value indicates that explicit state passing would be used
* `couch_rate_ets` - ets based global state store (ets tables are owned by app supervisor)
* `couch_rate_pd` - process dicionary based local state store

The "explicit state passing" style returns a tuple `{Result :: term(), state()}`.
The result is the same as for state store based API.


## State store based APIs of `couch_rate` module.

All functions can return `{error, Reason :: term()}` in case of errors.
This detail is ommited bellow.

* `create_if_missing(Id :: id(), Module :: module(), Store :: module(), Options :: map()) -> limiter()` - create new rate limiter instance
* `new(Id :: id(), Module :: module(), Store :: module(), Options :: map()) -> limiter()` - create new rate limiter instance
* `budget(limiter()) -> Budget :: integer().` - get batch size
* `delay(limiter()) -> Delay :: timeout().` - return delay in msec between batches
* `wait(limiter()) -> ok` - block the caller for amount of time returned by `delay/1`
* `in(limiter(), Reads :: integer()) -> limiter()` - notify rate limiter on the amount of reads were actually done (could be less than `budget`)
* `success(limiter(), Writes :: integer()) -> limiter()` - how many writes happen
* `failure(limiter()) -> limiter()` - called instead of `success/2` when failure happen
* `is_congestion(limiter()) -> boolean()` - returns `false` when congestion is detected
* `format(limiter()) -> [{Key :: atom(), Value :: term()}]` - return key value list representing important aspects of the limiter state
* `id(limitter()) -> id()` - returns `id()` of the rate limiter
* `module(limiter()) -> module()` - returns callback module implementing rate limiting logic.
* `state(limiter()) -> state()` - returns internal state of rate limiter.
* `store(limiter()) -> module() | nil` - returns store state backend.

# Testing

The test suite is written in Elixir.

## Running all tests

```
make couch && ERL_LIBS=`pwd`/src mix test --trace src/couch_rate/test/exunit/
```

## Running specific test suite

```
make couch && ERL_LIBS=`pwd`/src mix test --trace src/couch_rate/test/exunit/couch_rate_limiter_test.exs
```

## Running specific test using line number

```
make couch && ERL_LIBS=`pwd`/src mix test --trace src/couch_rate/test/exunit/couch_rate_limiter_test.exs:10
```

## Running traces with stats output

```
make couch && ERL_LIBS=`pwd`/src EXUNIT_DEBUG=true mix test --trace src/couch_rate/test/exunit/couch_rate_limiter_test.exs
```