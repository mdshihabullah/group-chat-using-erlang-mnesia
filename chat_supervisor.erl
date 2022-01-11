-module(chat_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_link_from_shell/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% Public function to start the server
-spec start_link_from_shell() -> 'true'.
start_link_from_shell() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  unlink(Pid).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link()->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []). 

%% The starting function, setting the parameters for the node to become a server-node,
%% herein setting the restart-strategy for the supervisor, and the parmeters of the child-node
init([]) ->
     io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),

     RestartStrategy = {one_for_one, 3, 3},
     
     ChildSpec = {
                  chat_server_proc,
                  {chat_server, start_link, []},
                  permanent,
                  infinity,
                  supervisor,
                  [chat_server]
                },
                
    {ok, {RestartStrategy,[ChildSpec]}}.