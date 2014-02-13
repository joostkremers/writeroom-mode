;;; writeroom-mode.el --- Minor mode for distraction-free writing  -*- lexical-binding: t -*-

;; Copyright (c) 2012 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 11 July 2012
;; Version: 2.0
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
(defvar writeroom-buffers nil
  "List of buffers in which writeroom-mode is activated.")
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

(define-obsolete-variable-alias 'writeroom-global-functions 'writeroom-global-effects "2.0")

(defcustom writeroom-global-effects '(writeroom-fullscreen writeroom-transparency writeroom-scroll-bar writeroom-menu-bar writeroom-tool-bar)
  "List of global effects for `writeroom-mode'.
These effects are enabled when `writeroom-mode' is activated in
the first buffer and disabled when it is deactivated in the last
buffer."
  :group 'writeroom
  :type '(set (const :tag "Fullscreen" writeroom-fullscreen)
              (const :tag "Disable transparency" writeroom-transparency)
              (const :tag "Disable scroll bar" writeroom-scroll-bar)
              (const :tag "Disable menu bar" writeroom-menu-bar)
              (const :tag "Disable tool bar" writeroom-tool-bar)))

(defcustom writeroom-major-modes '(text-mode)
  "List of major modes in which writeroom-mode is activated.
This option is only relevant when activating `writeroom-mode'
with `global-writeroom-mode'."
  :group 'writeroom
  :type '(repeat (symbol :tag "Major mode")))

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

(defun turn-on-writeroom-mode ()
  "Turn on `writeroom-mode'.
This function activates `writeroom-mode' in a buffer if that
buffer's major mode is a member of `writeroom-major-modes'."
  (if (memq major-mode writeroom-major-modes)
      (writeroom-mode 1)))

;;;###autoload
(define-minor-mode writeroom-mode
  "Minor mode for distraction-free writing."
  :init-value nil :lighter nil :global nil
  (if writeroom-mode
      (writeroom-enable)
    (writeroom-disable)))

;;;###autoload
(define-globalized-minor-mode global-writeroom-mode writeroom-mode turn-on-writeroom-mode
  :require 'writeroom-mode
  :group 'writeroom)

(defun writeroom-kill-buffer-function ()
  "Function to run when killing a buffer.
This function checks if `writeroom-mode' is enabled in the buffer
to be killed and adjusts `writeroom-buffers' and the global
effects accordingly."
  (when writeroom-mode
    (setq writeroom-buffers (delq (current-buffer) writeroom-buffers))
    (when (not writeroom-buffers)
      (writeroom-activate-global-effects nil))))

(add-hook 'kill-buffer-hook #'writeroom-kill-buffer-function)

(defun writeroom-activate-global-effects (arg)
  "Activate or deactivate global effects.
The effects are activated if ARG is non-nil, and deactivated
otherwise."
  (mapc #'(lambda (fn)
            (funcall fn arg))
        writeroom-global-effects))

(defun writeroom-adjust-window (&optional arg window)
  "Adjust WINDOW's margin and fringes.
If ARG is omitted or nil, the margins are set according to
`writeroom-width' and the fringes are disabled. If ARG is any
other value, the margins are set to 0 and the fringes are
enabled. WINDOW defaults to the selected window."
  ;; Note: this function is used in the buffer-local value of
  ;; window-configuration-change-hook, but only in buffers where
  ;; writeroom-mode is active, so we don't need to check if writeroom-mode
  ;; is really active.
  (or window
      (setq window (selected-window)))
  (if arg
      (progn
        (writeroom-set-margins window 0)
        (writeroom-set-fringes window t))
    (writeroom-set-margins window nil)
    (writeroom-set-fringes window nil)))

(defun writeroom-window-width (window)
  "Return the full width of WINDOW.
The full width is the width of the text area plus the width of
the margins."
  (let ((margins (window-margins window))
        (textarea (window-body-width window)))
    (+ textarea
       (or (car margins) 0)
       (or (cdr margins) 0))))

(defun writeroom-set-margins (window width)
  "Set/unset window margins for WINDOW.
If WIDTH is nil, the margins are set according to
`writeroom-width', otherwise the margins are set to WIDTH."
  (let* ((current-width (writeroom-window-width window))
         (margin (cond
                 ((integerp width) width)
                 ((integerp writeroom-width)
                  (/ (- current-width writeroom-width) 2))
                 ((floatp writeroom-width)
                  (/ (- current-width (truncate (* current-width writeroom-width))) 2)))))
    (set-window-margins window margin margin)))

(defun writeroom-set-fringes (window arg)
  "Enable or disable WINDOW's fringes.
If ARG is nil, the fringes are disabled. Any other value enables
them."
  (when writeroom-disable-fringe
    (if arg
        (set-window-fringes window nil nil)
      (set-window-fringes window 0 0))))

(defun writeroom-enable ()
  "Set up writeroom-mode for the current buffer.
This function sets the margins and disables the mode line and the
fringes. It also runs the functions in
`writeroom-global-effects' if the current buffer is the first
buffer in which `writeroom-mode' is activated."
  (when (not writeroom-buffers)
    (writeroom-activate-global-effects t))
  (add-to-list 'writeroom-buffers (current-buffer))
  (add-hook 'window-configuration-change-hook 'writeroom-adjust-window nil t)
  (when writeroom-disable-mode-line
    (setq writeroom-mode-line mode-line-format)
    (setq mode-line-format nil))
  ;; if the current buffer is displayed in some window, the windows'
  ;; margins and fringes must be adjusted.
  (mapc #'(lambda (w)
            (writeroom-adjust-window nil w))
        (get-buffer-window-list (current-buffer) nil)))

(defun writeroom-disable ()
  "Reset the current buffer to its normal appearance.
This function sets the margins to 0 and reenables the mode line
and the fringes. It also runs the functions in
`writeroom-global-effects' to undo their effects if
`writeroom-mode' is deactivated in the last buffer in which it
was active."
  (setq writeroom-buffers (delq (current-buffer) writeroom-buffers))
  (when (not writeroom-buffers)
    (writeroom-activate-global-effects nil))
  (remove-hook 'window-configuration-change-hook 'writeroom-adjust-window t)
  (when writeroom-disable-mode-line
    (setq mode-line-format writeroom-mode-line)
    (setq writeroom-mode-line nil))
  ;; if the current buffer is displayed in some window, the windows'
  ;; margins and fringes must be adjusted.
  (mapc #'(lambda (w)
            (writeroom-adjust-window t w))
        (get-buffer-window-list (current-buffer) nil)))

(provide 'writeroom-mode)

;;; writeroom-mode ends here
