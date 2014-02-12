# Writeroom-mode #

`writeroom-mode` is a minor mode for Emacs that implements a distraction-free writing mode similar to the famous Writeroom editor for OS X. `writeroom-mode` is meant for GNU Emacs 24.

Install `writeroom-mode.el` in the usual way (it is on [Melpa](http://melpa.milkbox.net/)) and activate it in a buffer by calling `M-x writeroom-mode RET`. By default, `writeroom-mode` does the following things:

* activate fullscreen
* disable transparency
* disable the menu bar
* disable the tool bar
* disable the scroll bar
* disable the fringes
* disable the mode line
* add window margins to the current buffer so that the text is 80 characters wide.

The last three effects are buffer-local. The other effects apply to the current frame. Because `writeroom-mode` is a minor mode, this isn't entirely on the up and up, since minor modes aren't supposed to have such global effects. But `writeroom-mode` is meant for distraction-free writing, so these effects do make sense.

All effects listed above can be switched off separately in the customization group `writeroom`. Fullscreen, transparency, scroll bar, menu bar and tool bar can be switched off by unchecking them in the option  `writeroom-global-functions`, the other effects have a corresponding toggle. Normally, the option fullscreen disables the window decorations and make the Emacs frame occupy the entire screen. If you prefer to "just" maximise the frame (i.e., keep the window decorations and do not cover the window manager's panel), you can set the option `writeroom-fullscreen-effect` to `maximized`.

The width of the text area is controlled by the option `writeroom-width`. It can be an absolute value, in which case it indicates the number of columns, or it can be a relative value. In that case, it should be a number between 0 and 1. Note that the width of the text area is based on the width of the window at the moment `writeroom-mode` is activated. If the window is resized, the text area width won't be adjusted. If that happens, restore the window width or deactivate and then reactivate `writeroom-mode`.

It is possible to activate `writeroom-mode` in more than one buffer. The global effects are of course activated only once and they remain active until `writeroom-mode` is deactivated in *all* buffers.

The default function to activate `writeroom-mode` is of course `writeroom-mode`. There is an alternative function, however: `writeroom-toggle`. This function also activates `writeroom-mode` in the current buffer, but when called in a buffer in which `writeroom-mode` is active, it disables `writeroom-mode` in *all* buffers in which it is active and also resets the global effects.
