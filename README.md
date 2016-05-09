CS583 Project
=============

This is the primary repository for our CS583 project.


Dependencies:
------------

- [SimpleIRC][1] : A small IRC networking library in haskell. To install this run:
```bash
cabal install simpleirc
```

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
- 3: Do the same processing as (2) but send the response as private message to sender

[1]: https://hackage.haskell.org/package/simpleirc-0.3.1/docs/
