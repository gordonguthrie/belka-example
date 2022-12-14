%%%-------------------------------------------------------------------
%% @doc belka example top level supervisor.
%%
%% In the example this supervisor is empty
%% when you build your own app you will add server to handle state
%% and start them here
%%
%% @end
%%%-------------------------------------------------------------------

-module(belka_example_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

% # Public OTP API

% This is a normal Erlang OTP application.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% when you build your own app you will develop your own specialist
% servers or even supervisor/server subsystem trees and you will
% start them all from here

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

% # There are no internal functions.