%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2015 11:27 AM
%%%-------------------------------------------------------------------
-module(resource_discovery).
-author("myang").

%% API
-export([add_target_resource_type/1, add_local_resource/2, trade_resources/0, fetch_resources/1]).

%%--------------------------------------------------------------------
%% @doc
%% Add request resource type to request list
%%
%% @end
%%--------------------------------------------------------------------
-spec add_target_resource_type(Type) -> any() when
  Type :: any().
add_target_resource_type(Type) ->
  rd_server:add_target_resource_type(Type).

%%--------------------------------------------------------------------
%% @doc
%% Add local resource type to supply list
%%
%% @end
%%--------------------------------------------------------------------
-spec add_local_resource(Type, Resource) -> any() when
  Type :: any(),
  Resource :: any().
add_local_resource(Type, Resource) ->
  rd_server:add_local_resource(Type, Resource).

%%--------------------------------------------------------------------
%% @doc
%% Fetch resources
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_resources(Type) -> any() when
  Type :: any().
fetch_resources(Type) ->
  rd_server:fetch_resources(Type).

%%--------------------------------------------------------------------
%% @doc
%% Trade resources with other nodes
%%
%% @end
%%--------------------------------------------------------------------
-spec trade_resources() -> any().
trade_resources() ->
  rd_server:trade_resources().