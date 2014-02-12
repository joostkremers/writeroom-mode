;;; writeroom-mode.el --- Minor mode for distraction-free writing  -*- lexical-binding: t -*-

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

;; writeroom-mode is a minor mode for Emacs that implements a
;; distraction-free writing mode similar to the famous Writeroom editor for
;; OS X. writeroom-mode is meant for GNU Emacs 24 and won't run on older
;; versions.
;;
;; See the README or info manual for usage instructions.
;; 
;;; Code:

;; bookkeeping
(defvar writeroom-buffers 0
  "Number of buffers in which writeroom-mode is activated.")
(defvar writeroom-fullscreen nil
  "Record fullscreen status before enabling `writeroom-mode'.")
(defvar writeroom-transparency nil
  "Record transparency statut before enabling `writeroom-mode'.")
(defvar writeroom-menu-bar nil
  "Record menu bar status before enabling `writeroom-mode'.")
(defvar writeroom-tool-bar nil
  "Record tool bar status before enabling `writeroom-mode'.")
(defvar writeroom-scroll-bar nil
  "Record scrollbar status before enabling `writeroom-mode'.")

(defgroup writeroom nil "Minor mode for distraction-free writing."
  :group 'wp
  :prefix "writeroom-")

(defcustom writeroom-width 80
  "Width of the writeroom writing area."
  :group 'writeroom
  :type '(choice (integer :label "Absolute width:")
                 (float :label "Relative width:" :value 0.5)))

(defcustom writeroom-disable-mode-line t
  "Whether to disable the mode line in writeroom buffers."
  :group 'writeroom
  :type 'boolean)

(defvar writeroom-mode-line nil
  "Contents of mode-line-format before disabling the mode line.
Used to restore the mode line after disabling writeroom-mode.")
(make-variable-buffer-local 'writeroom-mode-line)

(defcustom writeroom-disable-fringe t
  "Whether to disable the left and right fringes when writeroom is activated."
  :group 'writeroom
  :type 'boolean)  

(defcustom writeroom-fullscreen-effect 'fullboth
  "Effect applied when enabling fullscreen.
The value can be `fullboth', in which case fullscreen is
activated, or `maximized', in which case the relevant frame is
maximized but window decorations are still available."
  :group 'writeroom
  :type '(choice (const :tag "Fullscreen" fullboth)
                 (const :tag "Maximized" maximized)))

(defcustom writeroom-global-functions '(writeroom-fullscreen writeroom-transparency writeroom-scroll-bar writeroom-menu-bar writeroom-tool-bar)
  "List of functions with global effects for `writeroom-mode'.
These functions are called when `writeroom-mode' is activated in
the first buffer to enable the effects and again when it is
deactivated in the last buffer to disable them.

If you want to add your own function to this list, make sure that
it accepts one argument, which must be T to activate the effect
and NIL to deactivate it again. When the effect is deactivated,
the original configuration should be restored, so make sure to
save it when activating the effect."
  :group 'writeroom
  :type '(repeat function))

(defun writeroom-fullscreen (arg)
  "Turn fullscreen on/off."
  (if arg
      (progn
        (setq writeroom-fullscreen (frame-parameter nil 'fullscreen))
        (set-frame-parameter nil 'fullscreen writeroom-fullscreen-effect))
    (set-frame-parameter nil 'fullscreen writeroom-fullscreen)
    (setq writeroom-fullscreen nil)))

(defun writeroom-transparency (arg)
  "Turn transparency on/off."
  (if arg
      (progn
        (setq writeroom-transparency (frame-parameter nil 'alpha))
        (set-frame-parameter nil 'alpha '(100 100)))
    (set-frame-parameter nil 'alpha writeroom-transparency)
    (setq writeroom-transparency nil)))

(defun writeroom-menu-bar (arg)
  "Turn the menu bar on/off."
  (if arg
      (progn
        (setq writeroom-menu-bar (frame-parameter nil 'menu-bar-lines))
        (set-frame-parameter nil 'menu-bar-lines 0))
    (set-frame-parameter nil 'menu-bar-lines writeroom-menu-bar)
    (setq writeroom-menu-bar nil)))

(defun writeroom-tool-bar (arg)
  "Turn the tool bar on/off."
  (if arg
      (progn
        (setq writeroom-tool-bar (frame-parameter nil 'tool-bar-lines))
        (set-frame-parameter nil 'tool-bar-lines 0))
    (set-frame-parameter nil 'tool-bar-lines writeroom-tool-bar)
    (setq writeroom-tool-bar nil)))

(defun writeroom-scroll-bar (arg)
  "Turn the scroll bar on/off."
  (if arg
      (progn
        (setq writeroom-scroll-bar (frame-parameter nil 'scroll-bar-width))
        (set-frame-parameter nil 'scroll-bar-width 0))
    (set-frame-parameter nil 'scroll-bar-width writeroom-scroll-bar)
    (setq writeroom-scroll-bar nil)))

(defun writeroom-kill-buffer-function ()
  "Function to run when killing a buffer.
This function checks if `writeroom-mode' is enabled in the buffer
to be killed and adjusts `writeroom-buffers' and the global
effects accordingly."
  (when writeroom-mode
    (setq writeroom-buffers (1- writeroom-buffers))
    (when (= writeroom-buffers 0)
      (mapc #'(lambda (fn)
                (funcall fn nil))
            writeroom-global-functions))))

(add-hook 'kill-buffer-hook #'writeroom-kill-buffer-function)

;;;###autoload
(define-minor-mode writeroom-mode
  "Minor mode for distraction-free writing."
  :init-value nil :lighter nil :global nil
  (if writeroom-mode
      (writeroom-enable)
    (writeroom-disable)))

(defun writeroom-enable ()
  "Set up writeroom-mode for the current buffer.
This function runs the functions in `writeroom-global-functions'
if the current buffer is the first buffer in which
`writeroom-mode' is active. It also sets the margins of the
current buffer and disables the mode line and the fringes."
  (when (= writeroom-buffers 0)
    (mapc #'(lambda (fn)
	      (funcall fn t))
	  writeroom-global-functions))
  (setq writeroom-buffers (1+ writeroom-buffers))
  (let ((margin (cond
                 ((integerp writeroom-width)
                  (/ (- (window-body-width) writeroom-width) 2))
                 ((floatp writeroom-width)
                  (/ (- (window-body-width) (truncate (* (window-body-width) writeroom-width))) 2)))))
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
This function runs the functions in `writeroom-global-functions' to
undo their effects if `writeroom-mode' is deactivated in the last
buffer in which it was active. It also sets the margins of the current
buffer to 0 and reenables the mode line and the fringes."
  (setq writeroom-buffers (1- writeroom-buffers))
  (when (= writeroom-buffers 0)
    (mapc #'(lambda (fn)
	      (funcall fn nil))
	  writeroom-global-functions))
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
