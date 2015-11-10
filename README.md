# Writeroom-mode #

`writeroom-mode` is a minor mode for Emacs that implements a distraction-free writing mode similar to the famous Writeroom editor for OS X. `writeroom-mode` is meant for GNU Emacs 24, lower versions are not actively supported.


## Installation ##

`writeroom-mode` can be installed through the package manager from [Melpa](http://melpa.org/). If installing manually, make sure to also install `visual-fill-column`.


## Usage ##

`writeroom-mode` can be activated in a buffer by calling `M-x writeroom-mode RET`. By default, `writeroom-mode` does the following things:

* activate fullscreen
* disable transparency
* disable the menu bar
* disable the tool bar
* disable the scroll bar
* maximise the current window (i.e., delete all other windows in the frame)
* place the fringes outside the margins
* disable the mode line
* add window margins to the current buffer so that the text is 80 characters wide

The last three effects are buffer-local. The other effects apply to the current frame. Because `writeroom-mode` is a minor mode, this isn't entirely on the up and up, since minor modes aren't supposed to have such global effects. But `writeroom-mode` is meant for distraction-free writing, so these effects do make sense.

All these effects can be disabled or customised. In addition, there are several more options that are disabled by default but can be enabled in the customisation buffer.


## Customisation ##

### Global Writeroom Mode ###

Activate this option to automatically turn on `writeroom-mode` in any buffer that has one of the major modes in `writeroom-major-modes`.


### Border Width ###

Width of the border around the text area. Disabled by default, see `writeroom-global-effects` to enable the border.


### Fringes Outside Margins ###

If set, place the fringes outside the margins. `writeroom-mode` expands the window margins, causing the fringes to be pushed inside, which may be visually distracting. This option keeps the fringes at the window’s edges. Unset it if you prefer to have the fringes close to the text.


### Extra Line Spacing ###

Increase the line spacing. Can be an absolute value (the number of pixels to add to the line) or a number relative to the default line height. Disabled by default.


### Fullscreen Effect ###

Effect to apply when `writeroom-mode` activates fullscreen. Can be `fullboth`, which uses the entire screen (i.e., window decorations are disabled and the window manager’s panel or task bar is covered by the Emacs frame) or `maximized`, in which case the Emacs frame is maximised but keeps its window decorations and does not cover the panel.


### Global Effects ###

List of global effects:

- fullscreen
- transparency
- scroll bar
- menu bar
- tool bar
- border (add a border around the text area; disabled by default)
- sticky (display the window on all virtual workspaces; disabled by default)

Each option can be enabled or disabled individually.


### Major Modes ###

List of major modes in which `writeroom-mode` should be activated automatically. Use in conjunction with `global-writeroom-mode`.


### Maximize Window ###

Maximise the current window in its frame, i.e., delete all other windows.


### Restore Window Config ###

Restore the window configuration that existed before `writeroom-mode` was activated. This is primarily useful if you use `writeroom-mode` in only a single buffer, since the window configuration that is restored is the one that existed at the moment when `writeroom-mode` is called for the first time. Disabled by default.


### Width ###

Width of the text area. Can be specified as an absolute value (number of characters) or as a fraction of the total window width (in which case it should be a number between 0 and 1).


## The mode line ##

By default, `writeroom-mode` disables the mode line. Because the mode line can actually contain useful information, this may be impractical. For this reason, it is possible to use a customised mode line, which allows you to display information that is indispensable to you (e.g., the modified status, the file name).

If, for some reason, you need to look at the full mode line, you can use the command `writeroom-toggle-mode-line`. This command makes the mode line visible in the header line (at the top of the window). Calling it again hides the mode line. This command is bound to `s-?` (`s` is the super key, i.e., the Windows key on PCs, the ⌘ key on Macs), but it can be rebound by putting something like the following in your `init.el`:

```lisp
(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "s-?") nil)
  (define-key writeroom-mode-map (kbd "<some-key>") #’writeroom-toggle-mode-line))
```

The first `define-key` disables the binding for `s-?`. Substitute your preferred key binding in the second line to bind `writeroom-toggle-mode-line` to it.


## Multiple writeroom-mode buffers ##

It is possible to activate `writeroom-mode` in more than one buffer. The global effects are of course activated only once and they remain active until `writeroom-mode` is deactivated in *all* buffers. Alternatively, if you use `writeroom-mode` in multiple buffers with particular major modes (e.g., `text-mode`, `markdown-mode`), you can use the global minor mode `global-writeroom-mode`. This function enables the global effects and activates the buffer-local effects in all (current and future) buffers that have a major mode listed in the user option `writeroom-major-modes` (by default only `text-mode`).

When `global-writeroom-mode` is active, the function `writeroom-mode` can still be called to enable or disable `writeroom-mode` in individual buffers (regardless of their major mode, of course). Calling `global-writeroom-mode` again disables `writeroom-mode` in all buffers in which it is active, also those in which it was activated manually.


## Adding global effects ##

It is possible to define your own global effects and have them activated automatically when `writeroom-mode` is activated. For example, you may want to add your own font or colour effects, or replace the default fullscreen function with one that works in an older Emacs version. To do this, write a function that takes one argument and that activates the effect when this argument is `t` and deactivates it when it is `nil`. Then add this function to the user option `writeroom-global-effects` by checking the box "Custom effects" and adding the function to the list.
