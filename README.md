# Lispcord -- A Wrapper for the DiscordApp web-API

Lispcord aims to make it freakishly easy to build bots for Discord

The [examples] folder contains some more ideas on how to get started :)

## Ping bot
```lisp
(defpackage :ping-bot
  (:use :cl :lispcord))
(in-package :ping-bot)

(defvar *client* (make-bot <your-token-here>))
(connect *client*)

(watch-with-case (>message> msg)
  (:create (if (equal "ping" (lc:content msg))
               (create "pong" (lc:channel msg)))))
```

## Pipes:
Unlike many other libraries, which use basic event handlers, lispcord
abstracts over the entire dynamic through what it calls "pipes".

The `>message>` object above is a pipe, and it dispatches events in
a specific format (called "cargo")  which can be handled in various ways.
This allows the library to expose functions which can produce derivative
pipes from the source ones, like `pmap` (which works like `map` for lists)
or `pfilter` (which works like `remove-if-not`)