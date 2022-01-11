# Decentralized Chat System using Mnesia DBMS

This is a Distributed and decentralized Peer-to-Peer(P2P) group chat system using Mnesia (a distributed telecommunications DBMS)that supports exchanges of text messages between  multiple group(not individual) along with basic functionalities

## Instructions

To run the code, compile the files `chat_client`, `chat_server`, `chat_supervisor` & `db_logic` respectively.

To start the chat service, start erlang with a name and set cookies, and run `chat_client:start_link()`:

```erlang
> erl -sname alice -cookies 1234

(alice@DESKTOP-BCH0NID)1> chat_client:start_link().

{local,chat_supervisor} (<0.86.0>) starting... 
{local,chat_server} (<0.87.0>) starting...     
true
```

To connect another peer, start erlang with a name, and the same cookie, and run `chat_client:addNode(Host)`, where `Host` is any running peer (note this gives an error `{aborted,{already_exists,schema,'bob@DESKTOP-BCH0NID'}}`, but running `mnesia:info()` confirms all is as should be):

```erlang
> erl -sname bob -cookies 1234

(bob@DESKTOP-BCH0NID)1> chat_client:addNode('alice@DESKTOP-BCH0NID').

{local,chat_supervisor} (<0.103.0>) starting... 
{local,chat_server} (<0.104.0>) starting...
[{aborted,{already_exists,schema,'bob@DESKTOP-BCH0NID'}}]
```

**From any running peer** you can interact with the service via the following functions in `chat_client`:

- `addNode/1`: Add local node to the cluster if the chat has been started. i.e. this is *not* called from the node that initialises the chat-service. (`Host` must be a running peer)
- `createGroup/1`: Creates a new group with the given name
- `groupList/0` : Show all available group(s) as a list
- `sendMessage/2`: Sends a message to the given group
- `findUser/2`: Checks whether the user, `User` exists in the given group
- `findGroup/1`: Checks whether the group, `Group` exists
- `listUsers/1`: Lists all users in the given group
- `viewHistory/1`: Lists the chat history for the given group

## Examples

(After compiling as described)

### Starting the chat and connecting another peer

`shell 1`

```erlang
> erl -sname alice -cookies 1234

(alice@DESKTOP-BCH0NID)1> chat_client:start_link().

{local,chat_supervisor} starting... 
{local,chat_server} starting...     
true
```

`shell 2`

```erlang
> erl -sname bob -cookies 1234

(bob@DESKTOP-BCH0NID)1> chat_client:addNode('alice@DESKTOP-BCH0NID').

{local,chat_supervisor} starting... 
{local,chat_server} starting...
[{aborted,{already_exists,schema,'bob@DESKTOP-BCH0NID'}}]
```

### Creating a group

```erlang
(alice@DESKTOP-BCH0NID)2> chat_client:createGroup(birds). 
The group birds has been created.
ok
```

### Seeing the List of chat group(s)

This function is useful if the user forgets the group name or to see other group available as open chat channels

```erlang
(alice@DESKTOP-BCH0NID)10> chat_client:groupList().
---List of available groups--- 
 [birds,dinosaurs,mammals]
ok
```

### Sending a message to a group

Sending a message will result in the chat-history being shown to any peers, which have sent a message to the given group.

`shell 1`

```erlang
(alice@DESKTOP-BCH0NID)3> chat_client:sendMessage(birds, "Hello from Alice, the 
parrot :)").
You have sent your message to all users in group named 'birds'. 
 Users: ['alice@DESKTOP-BCH0NID']
---Chat history of group named 'birds'---
---START---
'Alice': 
 "Hello from Alice, the parrot :)" -- ['2021-05-22 12:17:02']

---END OF CHAT HISTORY--
ok
```

`shell 2`

```erlang
(bob@DESKTOP-BCH0NID)3> chat_client:sendMessage(birds, "Hello, Alice. Welcome to the group!").
You have sent your message to all users in group named 'birds'. 
 Users: ['bob@DESKTOP-BCH0NID','alice@DESKTOP-BCH0NID']
---Chat history of group named 'birds'---
---START---
'Alice': 
 "Hello from Alice, the parrot :)" -- ['2021-05-22 12:17:02']

'Bob': 
 "Hello, Alice. Welcome to the group!" -- ['2021-05-22 12:20:06']

---Chat history of group named 'birds'---
---START---
---END OF CHAT HISTORY--
ok
```

The message sent from bob, will result in the chat-history being shown to `alice`:

```erlang
(alice@DESKTOP-BCH0NID)5> 'Alice':
 "Hello from Alice, the parrot :)" -- ['2021-05-22 12:17:02']

(alice@DESKTOP-BCH0NID)5> 'Bob':
 "Hello, Alice. Welcome to the group!" -- ['2021-05-22 12:20:06']
```

### Searching for a user

```erlang
(alice@DESKTOP-BCH0NID)4> chat_client:findUser(birds, 'bob@DESKTOP-BCH0NID').
User named 'bob@DESKTOP-BCH0NID' exists. 
ok

(alice@DESKTOP-BCH0NID)5> chat_client:findUser(birds, 'mary@DESKTOP-BCH0NID').
User named 'mary@DESKTOP-BCH0NID' DOES NOT exist! 
ok
```

### Searching for a group

```erlang
(alice@DESKTOP-BCH0NID)6> chat_client:findGroup(birds). 
Group named 'birds' exists.
ok

(alice@DESKTOP-BCH0NID)7> chat_client:findGroup(wolves). 
Group named 'wolves' DOES NOT exist!
ok
```

### Listing users in a group

```erlang
(alice@DESKTOP-BCH0NID)8> chat_client:listUsers(birds). 

Users: ['bob@DESKTOP-BCH0NID','alice@DESKTOP-BCH0NID']
['bob@DESKTOP-BCH0NID','alice@DESKTOP-BCH0NID']
```

### Viewing chat-history for a group

```erlang
(alice@DESKTOP-BCH0NID)9> chat_client:viewHistory(birds).
---Chat history of group named 'birds'---
---START---
'Alice':
 "Hello from Alice, the parrot :)" -- ['2021-05-22 12:17:02']

'Bob':
 "Hello, Alice. Welcome to the group!" -- ['2021-05-22 12:20:06']

ok
```