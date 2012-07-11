# Writeroom-mode #

`writeroom-mode` is a minor mode for Emacs that implements a
distraction-free writing mode similar to the famous Writeroom editor for OS
X. `writeroom-mode` is meant for GNU Emacs 24 and won't run on older
versions.

## Usage ##

Install `writeroom-mode.el` in the usual way and activate it in a buffer by
calling `M-x writeroom-mode RET`. By default, `writeroom-mode` does the
following things:

* activate fullscreen
* disable transparency
* disable the tool bar
* disable the scroll bar
* disable the fringes
* disable the mode line
* add window margins to the current buffer so that the text is 80
  characters wide.

The last three effects are buffer-local. The other effects are global:
fullscreen and transparency apply to the current frame, the tool and scroll
bars apply to all frames. Because `writeroom-mode` is a minor mode, this
isn't entirely on the up and up, since minor modes aren't supposed to have
such global effects. But `writeroom-mode` is meant for distraction-free
writing, so these effects do make sense. Besides, if you're in the mood for
writing without distractions, you're not going to switch from the buffer
holding your text anyway, are you now? ;-)

All effects listed above can be switched off separately in the
customization group `writeroom`. Fullscreen and transparency can be
switched off by removing the relevant functions from
`writeroom-global-functions`, the other effects have a corresponding
toggle. The text width in a writeroom buffer can be changed from the
default 80 with the option `writeroom-width`.

Note that if you normally run Emacs with scroll and/or tool bar disabled,
you'll need to unset the options for disabling them in `writeroom-mode`,
otherwise they'll be turned on when you exit `writeroom-mode`.

It is possible to activate `writeroom-mode` in more than one buffer. The
global effects are of course activated only once and they remain active
until `writeroom-mode` is deactivated in *all* buffers.

The option `writeroom-global-functions` can be used to add additional
global effects. Just write a function for enabling and disabling the
relevant effect and add it to the list. See the doc string of this option
for some more details.
