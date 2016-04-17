-module(nif_util).
-export([
        hello/0
        ,concat/1
        ,alloc/1
    ]
).
-on_load(init/0).


% NIF functions end up overriding the functions defined in this module. But
% this module must define the functions we want the NIF to implement.
% Theoretically this won't ever get called as out on_load function init/0
% should raise an error if we have issues.
%
% A really nice person would make a pure Erlang fallback incase a NIF was
% unable to load for a specific platform.

hello() ->
    not_loaded(?LINE).

concat(_) ->
    not_loaded(?LINE).

alloc(_) ->
    not_loaded(?LINE).



%% Iternal functions

% Since we used init/0 in our -on_load() preprocessor directive, this
% function will get called as the module is loaded. This is the perfect
% place to load up our NIF shared library. Handily, the response of
% erlang:load_nif/2 matches the return specification for -on_load()
% functions.

init() ->
    SoName = filename:join([env:get(code_path), "priv", ?MODULE]),
    erlang:load_nif(SoName, 0).

% This is just a simple place holder. It mostly shouldn't ever be called
% unless there was an unexpected error loading the NIF shared library.

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

