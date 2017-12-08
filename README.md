# Lispcord -- A Wrapper for the DiscordApp web-API

Lispcord aims to make it freakishly easy to build bots for Discord

The [examples] folder contains some more ideas on how to get started :)

#### NOTE: this is not-even alpha quality software, a lot of the api is chaning
rapidly, use at own risk!

## Ping bot

This assumes that :lispcord has been loaded in your image. If not, try running
`(ql:quickload :lispcord)` after cloning the repo to your ~/common-lisp folder

```lisp
(defpackage :ping-bot
  (:use :cl :lispcord))
(in-package :ping-bot)

(defbot *ping-bot* "<Your Token Here>")
(connect *ping-bot*)

(defpipe >event> :for msg :from >message-create>
         :do (if (equal (lc:content msg) "ping")
	         (reply msg "pong")))
```

## Pipes:
Unlike many other libraries, which use basic event handlers, lispcord
abstracts over the entire dynamic through what it calls "pipes".

The `>message-create>` object above is a pipe, and it dispatches events  which can be handled in various ways.
This allows you to define your own new (or derivative) pipes,
to chain them, filter them or otherwise combine them without being
forced into a single monolithic event-dispatching mechanism.