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

All effects listed above can be switched off separately in the customization group `writeroom`. Fullscreen, transparency, scroll bar, menu bar and tool bar can be switched off by unchecking them in the option  `writeroom-global-functions`, the other effects have a corresponding toggle. Normally, the option fullscreen disables the window decorations and make the Emacs frame occupy the entire screen. If you prefer to "just" maximise the frame (i.e., keep the window decorations and do not cover the window manager's panel), you can set the option `writeroom-fullscreen-effect` to `maximized`. The width of the text area is controlled by the option `writeroom-width`. It can be an absolute value, in which case it indicates the number of columns, or it can be a relative value. In that case, it should be a number between 0 and 1.

It is possible to activate `writeroom-mode` in more than one buffer. The global effects are of course activated only once and they remain active until `writeroom-mode` is deactivated in *all* buffers.

Apart from the buffer-local minor mode, there is also a global minor mode, appropriately called `global-writeroom-mode`. This function can be called to activate `writeroom-mode` in *all* buffers with specific major modes, as defined by the user option `writeroom-major-modes`. When `global-writeroom-mode` is active, the function `writeroom-mode` can still be called to enable or disable `writeroom-mode` in individual buffers. Calling `global-writeroom-mode` again disables `writeroom-mode` in all buffers in which it is active, even those in which it was activated manually.
