;;; -*- lexical-binding: t -*-

;; [[file:emacs.org::*Garbage Collection][Garbage Collection:1]]
(setq gc-cons-threshold #x40000000)

;; Set the maximum output size for reading process output, allowing for larger data transfers.
(setq read-process-output-max (* 1024 1024 4))

;; Don't load package.el since we're using Straight
(setq package-enable-at-startup nil)

(setq default-frame-alist
	'((width . 100)   ; Width in characters
	  (height . 30))) ; Height in lines
;; Garbage Collection:1 ends here
