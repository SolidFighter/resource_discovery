%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2015 11:20 AM
%%%-------------------------------------------------------------------
-module(rd_server).
-author("myang").

-behaviour(gen_server).

%% API
-export([start_link/0, add_target_resource_type/1, add_local_resource/2, fetch_resources/1, trade_resources/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types,
                local_resource_tuples,
                found_resource_tuples}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Add request resource type to request list
%%
%% @end
%%--------------------------------------------------------------------
-spec add_target_resource_type(Type) -> any() when
  Type :: any().
add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

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
  gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

%%--------------------------------------------------------------------
%% @doc
%% Fetch resources
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_resources(Type) -> any() when
  Type :: any().
fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

%%--------------------------------------------------------------------
%% @doc
%% Trade resources with other nodes
%%
%% @end
%%--------------------------------------------------------------------
-spec trade_resources() -> any().
trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{target_resource_types = [],
              local_resource_tuples = dict:new(),
              found_resource_tuples = dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.found_resource_tuples), State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({add_target_resource_type, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Resource}}, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  NewResourceTuples = add_resource(Type, Resource, ResourceTuples),
  {noreply, State#state{local_resource_tuples = NewResourceTuples}};
handle_cast(trade_resources, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  AllNodes = [node() | nodes()],
  lists:foreach(
    fun(Node) ->
      gen_server:cast({?SERVER, Node},
                      {trade_resources, {node(), ResourceTuples}})
    end,
    AllNodes),
  {noreply, State};
handle_cast({trade_resources, {ReplyTo, Remotes}},
            #state{local_resource_tuples = Locals,
                   target_resource_types = TargetTypes,
                   found_resource_tuples = OldFound} = State) ->
  FilteredRemotes = resources_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      gen_server:cast({?SERVER, ReplyTo},
                      {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_tuples = NewFound}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_resource(Type, Resource, ResourceTuples) ->
  case dict:find(Type, ResourceTuples) of
    {ok, ResourceList} ->
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      dict:store(Type, NewList, ResourceTuples);
    error ->
      dict:store(Type, [Resource], ResourceTuples)
  end.

resources_for_types(Types, ResourceTuples) ->
  Fun =
    fun(Type, Acc) ->
      case dict:find(Type, ResourceTuples) of
        {ok, List} ->
          [{Type, Resource} || Resource <- List] ++ Acc;
        error ->
          Acc
      end
    end,
  lists:foldl(Fun, [], Types).

add_resources([{Type, Resource} | T], ResourceTuples) ->
  add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
  ResourceTuples.