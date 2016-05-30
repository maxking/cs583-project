hasbot (CS583 Project)
=============
[![Build Status](https://travis-ci.org/maxking/cs583-project.svg?branch=master)](https://travis-ci.org/maxking/cs583-project)

This is an IRC bot that exposes haskell functions over a chat interface.
More details can be found in Proposal.doc

Link to the project: https://github.com/maxking/cs583-project

Project Members:
- Abhilash Raj
- Deepthi S Kumar

Dependencies
------------

- [SimpleIRC][1] : A small IRC networking library in haskell.
- Parsec

How to Run
----------

To run this project, you can build this project by:

```bash
$ git clone https://github.com/maxking/cs583-project
$ cd cs583-project
$ cabal build
```

Then you can run the program like:

```bash
$ ./dist/build/hasbot/hasbot
```

This will run the bot and connect it to IRC (Internet Relay Chat) network. To
interact with the bot, open an irc client or a [web client][2] and join the
channel `##maxking` and you will find the bot there with nick `hasbot`.

For now, the bot just responds to simple IRC commands like
```
> !add 3 4
7

> !neg -9
9
```

Anything else that you type is echo'd back from the bot.

Features
--------

- hasbot connects to `Frenode` network by default and joins `##maxking` channel
- It logs all the conversation on the channel internally
- Logs are accesible through web at `http://localhost:3000`
- The webserver and irc bot run in two seperate threads that are created using
  [forkIO][3].
- hasbot responds to the commands on the channel `##maxking` and also over
  private message.


TODO
----

- Add a	`help` command to help users with all the available commands.
- Make it easy to add more commands (right now only !add and !neg
  commands are available)
- Allow searching through the history either as a command or over the web
  interface
- Proper error handling, in case of the arguments passed for a commands cause
  the ccommand to error out.  

Internal Design
---------------

All messages are of type `IrcMessage` and have following important attributes:

- `mNick` : Nickname of the sender
- `mUser` : Realname of the sender
- `mHost` : Hostname of the sender
- `mServer`: Server address of the sender
- `mCode` : IRC code for the message, almost always defaults to `PRIVMSG`
- `mChan` : Sent to, either a channel or to the channel or nick
- `mOrigin` : Sent by, if private message then sender else channel
- `mRaw` : Raw message.

When a message is recived, it can be of three types:

1. Sent to the channel
2. Sent to the channel but starting with `hasbot:` to highlight
3. Sent as private message to the bot

In each of the three cases above we perform the following actions:

- 1: Just log the message to a file
- 2: Parse the command in the message and respond to the sender with a response
    on the channel that the command was recieved on
- 3: Do the same processing as (2) but send the response as private message to
  sender

Design Questions
----------------

- Currently the commands take list of strings as arguments. These arguments are
  then type casted to the type required by the actual function that executes the
  command. what is the best way to specify the number of arguments and type of
  the arguments so that it can be type checked?

- What to do about the messages to the channel directly without highlight
  excecpt for logging?

- How to make the design modular so that commands can be added over the chat
  interface itself.



[1]: https://hackage.haskell.org/package/simpleirc-0.3.1/docs/
[2]: https://webchat.freenode.net/
[3]: https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Concurrent.html#v:forkIO
