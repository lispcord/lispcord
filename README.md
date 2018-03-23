# Lispcord -- A Wrapper for the DiscordApp web-API

Lispcord aims to make it freakishly easy to build bots for Discord

The [examples] folder contains some more ideas on how to get started :)


#### NOTE: this is not-even alpha quality software
a lot of the api is changing rapidly, use at own risk!

#### NOTE2.0: recently we axed the Pipe system
please re-adjust your bots to use the new API going forwards.
As it turned out, the pipes worked well for small bots but had the penchant to
bloat in complexity rather quickly, and weren't particularly fast :D


## Ping bot

This assumes that :lispcord has been loaded in your image. If not, try running
`(ql:quickload :lispcord)` after cloning the repo to your ~/common-lisp or
~/quicklisp/local-projects folder

```lisp
(defpackage :ping-bot
  (:use :cl :lispcord))
(in-package :ping-bot)

(set-log-level :info)

(defbot *ping-bot* "<Your Token Here>")
(connect *ping-bot*) ; Yes, you can register handlers after connect

(add-event-handler :on-message-create
  (lambda (msg) (if (string= (lc:content msg) "ping!") (reply msg "pong!"))))
```

Unlike many other libraries, lispcord is capable of running an arbitrary amount
of client-instances at the same time.  
Every (non-cache related) function takes an optional "bot" parameter,
either via keyword or as an anonymous optional,
with which you can specify what instance should execute the action.  
For convinience, however, lispcord *also* defines a dynamic `*CLIENT*` which
gets automatically bound to the last instance defined via `DEFBOT`.  
This allows you to
  A: drop having to specifiy the bot for 1-instance scripts and
  B: lets you use `LET` to override the global, and create local 1-instance spaces
