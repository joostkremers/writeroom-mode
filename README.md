# Writeroom-mode #

`writeroom-mode` is a minor mode for Emacs that implements a distraction-free writing mode similar to the famous Writeroom editor for OS X. `writeroom-mode` is meant for GNU Emacs 24 and won't run on older versions.

## Usage ##

Install `writeroom-mode.el` in the usual way and activate it in a buffer by calling `M-x writeroom-mode RET`. By default, `writeroom-mode` does the following things:

* activate fullscreen
* disable transparency
* disable the tool bar
* disable the scroll bar
* disable the fringes
* disable the mode line
* add window margins to the current buffer so that the text is 80 characters wide.

The last three effects are buffer-local. The other effects apply to the current frame. Because `writeroom-mode` is a minor mode, this isn't entirely on the up and up, since minor modes aren't supposed to have such global effects. But `writeroom-mode` is meant for distraction-free writing, so these effects do make sense. Besides, if you're in the mood for writing without distractions, you're not going to switch from the buffer holding your text anyway, are you now? ;-)

All effects listed above can be switched off separately in the customization group `writeroom`. Fullscreen, transparency, scroll-bar and tool-bar can be switched off by removing the relevant functions from `writeroom-global-functions`, the other effects have a corresponding toggle. The width of the text area is controlled by the option `writeroom-width`. It can be an absolute value, in which case it indicates the number of columns, or it can be a relative value. In that case, it should be a number between 0 and 1.

Note that the width of the text area is based on the width of the window at the moment `writeroom-mode` is activated. If the window is resized, the text area width won't be adjusted. If that happens, restore the window width or deactivate and then reactivate `writeroom-mode`.

It is possible to activate `writeroom-mode` in more than one buffer. The global effects are of course activated only once and they remain active until `writeroom-mode` is deactivated in *all* buffers.

The option `writeroom-global-functions` can be used to add additional global effects. Just write a function for enabling and disabling the relevant effect and add it to the list. See the doc string of this option for some more details.

## Fullscreen limitations ##

Fullscreen as implemented here only works on Linux and, as of Emacs 24.3, on OS X. For tips on getting a fullscreen Emacs on Windows or older Emacsen, see: <http://emacswiki.org/emacs/FullScreen>
