-module(chat_client).

%% Internal
-export([createGroup/1, sendMessage/2, viewHistory/1, listUsers/1, findUser/2, findGroup/1, start_link/0, addNode/1, groupList/0]).
%% Remote
-export([remote_createGroup/2, remote_sendMessage/4, remote_viewHistory/2, remote_listUsers/2, remote_findUser/3, remote_findGroup/2]).
%% Helpers
-export([get_server/0, connect_client/1]).


%% ======================================================
%%               Internal Calls
%% ======================================================
%% These function can only be called internally from a server-node

%% Start link from local node
-spec start_link() -> 'true'.
start_link() ->
    chat_supervisor:start_link_from_shell().

%% Add local node to cluster. 'Host' must be one of the running_db_nodes in the cluster
-spec addNode(atom()) -> any().
addNode(Host) ->
    mnesia:start(),
    chat_supervisor:start_link_from_shell(),
    rpc:call(Host, db_logic, addReplica, [node()]).

%% Creates a new group with the name group.
-spec createGroup(atom()) -> {'aborted',_} | {'atomic','ok'}.
createGroup(Group) ->
    chat_server:newGroup(Group).

%% Sends a message to all users in the group with name Group.
-spec sendMessage(atom(), string()) -> 'ok'.
sendMessage(Group, Message) ->
    chat_server:sendMessage(Group, node(), Message),
    GroupUsers = chat_server:users(Group),
    showHistoryToAll(GroupUsers, Group).

%% Checks if a user exists, by userame.
-spec findUser(atom(), atom()) -> boolean().
findUser(Group, User) ->
    chat_server:findUser(Group, User).

%% Checks if a group exists, by group name.
-spec findGroup(atom()) -> boolean().
findGroup(Group) ->
    chat_server:findGroup(Group).

%% List all Users that is connected to the group
-spec listUsers(atom()) -> ['atomic'].
listUsers(Group) ->
    chat_server:users(Group).

%% List all chat group(s) in system
-spec groupList() -> 'ok'.
groupList() ->
    GroupList = lists:delete(schema,mnesia:system_info(tables)),
    io:format("---List of available groups--- ~n ~p~n",[GroupList]).

%% Lists the history of the chats in the group
-spec viewHistory(atom()) -> any().
viewHistory(Group) ->
    io:format("---Chat history of group named '~p'---~n",[Group]),
    io:format("---START--- ~n"),
    chat_server:history(Group).

%% ======================================================
%%              Remote Calls
%% ======================================================
%% These functions are intended to be called from a remote client-node
%% Can also be called from a server-node

%% Remote version of createGroup.
remote_createGroup(Server, Group) ->
    rpc:call(Server, chat_client, createGroup, [Group]).

%% remote version of sendMessage.
remote_sendMessage(Server, Group, User, Message) ->
    rpc:call(Server, chat_client, sendMessage, [Group, User, Message]).

%% Remote version of findUser.
remote_findUser(Server, Group, User) ->
    rpc:call(Server, chat_client, findUser, [Group, User]).

%% Remote version of findGroup.
remote_findGroup(Server, Group) ->
    rpc:call(Server, chat_client, findGroup, [Group]).

%% Remote version of listUsers.
remote_listUsers(Server, Group) ->
    rpc:call(Server, chat_client, listUsers, [Group]).

%% Remote version of viewHistory.
remote_viewHistory(Server, Group) ->
    io:format("---Server:~p~n",[Server]),
    rpc:call(Server, chat_client, viewHistory, [Group]).


%% ======================================================
%%              Helpers
%% ======================================================

% Helper function for showing the chat history in all other users terminals, within a given group.
% It is used for sendMessage.
showHistoryToAll([], _) ->
    io:format("---END OF CHAT HISTORY--~n");

showHistoryToAll([User|GroupUsers], Group) ->
    rpc:call(User, chat_client, viewHistory, [Group]),
    showHistoryToAll(GroupUsers, Group).


%% Helper functions to connect a client to the server

%% Returns a running server to the client
connect_client(Host) ->
    rpc:call(Host, chat_client, get_server, []).

%% Selects a random node from one of the servers, to be used as dedicated server for a client
%% Works as a load-balancer
get_server() ->
    Servers = mnesia:table_info(db_logic, where_to_write),
    lists:nth(rand:uniform(length(Servers)), Servers).


