(in-package :lispcord.constants)

(defvar +os+ (software-type)
  "The Operating system the library was compiled on")

(defvar +lib+ "lispcord"
  "This library")

(defvar +base-url+ "https://discordapp.com/api/v6/"
  "The api path the library targets.
This is made constant because the library implementation won't be able
to adjust to the various apis anyways")

(defvar +api-suffix+ "?v=6&encoding=json"
  "The api suffix. See +BASE-URL+ on why this is a constant")

(defvar +gw-rate-limit+ 120
  "The gateway gives no warning whatsoever about ratelimit violations.
Instead, we have to track the amount of calls manually")

(defvar +gw-rate-limit-connection+ 5
  "Discord also limits the connections/second")

(defvar +gw-rate-limit-game-status+ 5
  "As well as the game status updates/minute")
