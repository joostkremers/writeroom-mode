;;; -*- lexical-binding: t -*-
;;; writeroom-mode.el --- Minor mode for distraction-free writing

;; Copyright (c) 2012 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 11 July 2012
;; Version: 1.0.0
;; Keywords: text

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This minor mode implements a distraction-free writing mode similar to
;; the famous Writeroom editor for OS X.
;;
;; # Usage #
;;
;; Install this file in the usual way and activate it in a buffer by
;; calling `M-x writeroom-mode RET`. By default, `writeroom-mode` does the
;; following things:
;;
;; * activate fullscreen
;; * disable transparency
;; * disable the tool bar
;; * disable the scroll bar
;; * disable the fringes
;; * disable the mode line
;; * add window margins to the current buffer so that the text is 80
;;   characters wide.
;;
;; The last three effects are buffer-local. The other effects are global:
;; fullscreen and transparency apply to the current frame, the tool and
;; scroll bars apply to all frames. Because `writeroom-mode` is a minor
;; mode, this isn't entirely on the up and up, since minor modes aren't
;; supposed to have such global effects. But `writeroom-mode` is meant for
;; distraction-free writing, so these effects do make sense. Besides, if
;; you're in the mood for writing without distractions, you're not going to
;; switch from the buffer holding your text anyway, are you now? ;-)
;;
;; All effects listed above can be switched off separately in the
;; customization group `writeroom`. Fullscreen and transparency can be
;; switched off by removing the relevant functions from
;; `writeroom-global-functions`, the other effects have a corresponding
;; toggle. The text width in a writeroom buffer can be changed from the
;; default of 80 with the option `writeroom-width`.
;;
;; Note that if you normally run Emacs with scroll and/or tool bar
;; disabled, you'll need to unset the options for disabling them in
;; `writeroom-mode`, otherwise they'll be turned on when you exit
;; `writeroom-mode`.
;;
;; The option `writeroom-global-functions` can be used to add additional
;; global effects. Just write a function for enabling and disabling the
;; relevant effect and add it to the list. See the doc string of this
;; option for some more details.
;;
;; It is possible to activate `writeroom-mode` in more than one buffer. The
;; global effects are of course activated only once and they remain active
;; until `writeroom-mode` is deactivated in *all* buffers.
;;
;; The code for `writeroom-mode` is available on github:
;; <https://github.com/joostkremers/writeroom-mode.git>

;;; Code:

(defvar writeroom-buffers 0
  "Number of buffers in which writeroom-mode is activated.")

(defgroup writeroom nil "Minor mode for distraction-free writing."
  :group 'wp
  :prefix "writeroom-")

(defcustom writeroom-width 80
  "*Width of the writeroom writing area."
  :group 'writeroom
  :type '(integer :label "Width:"))

(defcustom writeroom-disable-mode-line t
  "*Whether to disable the mode line in writeroom buffers."
  :group 'writeroom
  :type 'boolean)

(defvar writeroom-mode-line nil
  "Contents of mode-line-format before disabling the mode line.
Used to restore the mode line after disabling writeroom-mode.")
(make-variable-buffer-local 'writeroom-mode-line)

(defcustom writeroom-disable-tool-bar t
  "*Whether to disable the tool bar when writeroom is activated."
  :group 'writeroom
  :type 'boolean)

(defcustom writeroom-disable-scroll-bar t
  "*Whether to disable the scroll bar when writeroom is activated."
  :group 'writeroom
  :type 'boolean)

(defcustom writeroom-disable-fringe t
  "*Whether to disable the left and right fringes when writeroom is activated."
  :group 'writeroom
  :type 'boolean)  

(defcustom writeroom-global-functions '(writeroom-fullscreen writeroom-transparency)
  "*List of functions with global effects for writeroom-mode.
These functions are called when writeroom is activated to enable
the effects and again when it is deactivated to disable them.

If you want to add your own function to this list, make sure that
it accepts one argument, which must be T to activate the effect
and NIL to deactivate it again. When the effect is deactivated,
the original configuration should be restored, so make sure to
save it when activating the effect."
  :group 'writeroom
  :type '(repeat function))

(let (fullscreen)
  (defun writeroom-fullscreen (arg)
    "Turn fullscreen on/off."
    (if arg
	(progn
	  (setq fullscreen (frame-parameter nil 'fullscreen))
	  (set-frame-parameter nil 'fullscreen 'fullboth))
      (set-frame-parameter nil 'fullscreen fullscreen)
      (setq fullscreen nil))))

(let (transparency)
  (defun writeroom-transparency (arg)
    "Turn transparency on/off."
    (if arg
	(progn
	  (setq transparency (frame-parameter nil 'alpha))
	  (set-frame-parameter nil 'alpha '(100 100)))
      (set-frame-parameter nil 'alpha transparency)
      (setq transparency nil))))

;;;###autoload
(define-minor-mode writeroom-mode
  "Minor mode for distraction-free writing."
  :init-value nil :lighter nil :global nil
  (if writeroom-mode
      (writeroom-enable)
    (writeroom-disable)))

(defun writeroom-enable ()
  "Set up writeroom-mode for the current buffer.
This function runs the functions in WRITEROOM-GLOBAL-FUNCTIONS,
sets the margins of the current buffer and disables the mode
line and the fringes."
  (when (= writeroom-buffers 0)
    (mapc #'(lambda (fn)
	      (funcall fn t))
	  writeroom-global-functions)
    (when writeroom-disable-scroll-bar
      (scroll-bar-mode -1))
    (when writeroom-disable-tool-bar
      (tool-bar-mode -1)))
  (setq writeroom-buffers (1+ writeroom-buffers))
  (let ((margin (/ (- (window-body-width) writeroom-width) 2)))
    (setq left-margin-width margin
	  right-margin-width margin))
  (when writeroom-disable-fringe
    (setq left-fringe-width 0
	  right-fringe-width 0))
  (when writeroom-disable-mode-line
    (setq writeroom-mode-line mode-line-format)
    (setq mode-line-format nil))
  (set-window-buffer nil (current-buffer)))

(defun writeroom-disable ()
  "Reset the current buffer to its normal appearance.
This function runs the functions in WRITEROOM-GLOBAL-FUNCTIONS to
undo their effects, sets the margins of the current buffer to 0
and reenables the mode line and the fringes."
  (setq writeroom-buffers (1- writeroom-buffers))
  (when (= writeroom-buffers 0)
    (mapc #'(lambda (fn)
	      (funcall fn nil))
	  writeroom-global-functions)
    (when writeroom-disable-scroll-bar
      (scroll-bar-mode 1))
    (when writeroom-disable-tool-bar
      (tool-bar-mode 1)))
  (setq left-margin-width 0
	right-margin-width 0)
  (when writeroom-disable-fringe
    (setq left-fringe-width nil
	  right-fringe-width nil))
  (when writeroom-disable-mode-line
    (setq mode-line-format writeroom-mode-line)
    (setq writeroom-mode-line nil))
  (set-window-buffer nil (current-buffer)))

(provide 'writeroom-mode)

;;; writeroom-mode ends here
