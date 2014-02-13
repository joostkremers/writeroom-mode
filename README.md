# Writeroom-mode #

`writeroom-mode` is a minor mode for Emacs that implements a distraction-free writing mode similar to the famous Writeroom editor for OS X. `writeroom-mode` is meant for GNU Emacs 24, lower versions are not actively supported.

## Installation ##

`writeroom-mode` can be installed by putting `writeroom-mode.el` in your `load-path` and calling `(load "writeroom")` in your `init.el` or, more conveniently, through the package manager: `writeroom-mode` is available on [Melpa](http://melpa.milkbox.net/).

## Usage ##

`writeroom-mode` can be activated in a buffer by calling `M-x writeroom-mode RET`. By default, `writeroom-mode` does the following things:

* activate fullscreen
* disable transparency
* disable the menu bar
* disable the tool bar
* disable the scroll bar
* disable the fringes
* disable the mode line
* add window margins to the current buffer so that the text is 80 characters wide.

The last three effects are buffer-local. The other effects apply to the current frame. Because `writeroom-mode` is a minor mode, this isn't entirely on the up and up, since minor modes aren't supposed to have such global effects. But `writeroom-mode` is meant for distraction-free writing, so these effects do make sense.

## Customisation ##

All `writeroom-mode` effects can be switched off separately in the customisation group `writeroom`. The option `writeroom-global-effects` lists the global effects (fullscreen, transparency, scroll bar, menu bar and tool bar), which can be switched off individually. The fringes and the mode line each have a toggle option.

Normally, the option fullscreen disables the window decorations and make the Emacs frame occupy the entire screen, covering the window manager's panel or task bar. If you prefer to "just" maximise the frame (i.e., keep the window decorations and do not cover the window manager's panel), you can set the option `writeroom-fullscreen-effect` to `maximized`.

Lastly, the width of the text area is controlled by the option `writeroom-width`. It can be an absolute value, in which case it indicates the number of columns, or it can be a percentage of the window's width. In that case, it should be a number between 0 and 1.

## Multiple writeroom-mode buffers ##

It is possible to activate `writeroom-mode` in more than one buffer. The global effects are of course activated only once and they remain active until `writeroom-mode` is deactivated in *all* buffers. Alternatively, if you use `writeroom-mode` in multiple buffers with particular major modes (e.g., `text-mode`, `markdown-mode`), you can use the global minor mode `global-writeroom-mode`. This function enables the global effects and activates the buffer-local effects in all (current and future) buffers that have a major mode listed in the user option `writeroom-major-modes` (by default only `text-mode`).

When `global-writeroom-mode` is active, the function `writeroom-mode` can still be called to enable or disable `writeroom-mode` in individual buffers. Calling `global-writeroom-mode` again disables `writeroom-mode` in all buffers in which it is active, also those in which it was activated manually.

## Adding global effects ##

It is possible to define your own global effects and have them activated automatically when `writeroom-mode` is activated, e.g., certain font or colour effects. To do this, write a function that takes one argument and that activates the effect when this argument is `t` and deactivates it when it is `nil`. Then add this function to the user option `writeroom-global-effects` by checking the box "Custom effects" and adding the function to the list.

## New in version 2 ##

* Add a global minor mode.
* Automatically adjust margins when window width changes.
* Rename `writeroom-global-functions` to `writeroom-global-effects`.
