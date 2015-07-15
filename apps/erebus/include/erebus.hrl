-type codec() :: pfor | pford | cypress.

-record(window, {
    %% Interval, in seconds
    i :: pos_integer(),
    %% In-memory storage retention, in seconds
    mem :: pos_integer(),
    %% On-disk storage retention, in seconds
    disk :: pos_integer(),
    %% Codec module
    codec :: codec(),
    %% any() :: NIF datastructure
    data :: any() | undefined
}).

-type window() :: #window{}.

-record(metric, {
    id :: binary(),
    int :: list({binary(), binary()}),
    ext :: list({binary(), binary()})
}).

-type metric() :: #metric{}.

-type timestamp() :: pos_integer().
-type value() :: number() | null.
-type interval() :: pos_integer().
