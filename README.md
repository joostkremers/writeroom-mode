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

All effects listed above can be switched off separately in the customization group `writeroom`. Fullscreen, transparency, scroll-bar and tool-bar can be switched off by removing the relevant functions from `writeroom-global-functions`, the other effects have a corresponding toggle. The text width in a writeroom buffer can be changed from the default of 80 with the option `writeroom-width`.

It is possible to activate `writeroom-mode` in more than one buffer. The global effects are of course activated only once and they remain active until `writeroom-mode` is deactivated in *all* buffers.

The option `writeroom-global-functions` can be used to add additional global effects. Just write a function for enabling and disabling the relevant effect and add it to the list. See the doc string of this option for some more details.

## Fullscreen limitations ##

Fullscreen as implemented here only works on Linux. It can be made to work on OS X as well, if you compile Emacs with the patches from <ftp://ftp.math.s.chiba-u.ac.jp/emacs/>. Alternatively, OS X has its own way of making windows fullscreen: <http://support.apple.com/kb/PH4530>.

More tips on getting a fullscreen Emacs can be found here: <http://emacswiki.org/emacs/FullScreen>

