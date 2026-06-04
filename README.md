---
title: Emacs for the rest of us
---


This is an [Emacs](https://www.gnu.org/software/emacs/) configuration.
There are many like it, but this one is mine. If you like it, make it
yours! It provides lots of functionality while keeping things light
and fast. It tries to tames Emacs, making it behave more like other
applications you use.

Project goals and philosophy
========================================

The main goal of this project is to provide an Emacs configuration
that works more or less they way you would expect an editor or IDE to
work in the second decade of the twenty-first century, without losing
the things that make Emacs special. The included packages were
selected with social scientists in mind (e.g., it includes support for
R, Stata, Python, Markdown, and LaTeX).

The overarching philosophy is pragmatism; we're trying to make Emacs
as useful as possible, and to reduce the time needed to start using
Emacs productively.

Feature highlights
==========================

Highlights of this Emacs configuration:
-   Literate programming configuration for running R, python, or other
    programming languages inside markdown or org-mode files.
-   Consistent and familiar code evaluation using `C-ret` (that's `Control + Return`).
-   Consistent indentation and folding using the `tab` key.
-   Consistent completion using `<tab>`.
-   Support for LaTeX and other markup languages.
-   Powerful and simple search-based tools for finding commands, files
    and buffers, inserting citations etc.
-   More standard select/copy/paste keys and right-click behavior makes
    it more familiar to those new to Emacs.
-   Multiple cursors, as in Sublime and VScode
-   Convenient window management.
-   **Magit** for Git: status, diffs, and history from inside Emacs (see
    [Version control](#version-control-magit) below).
-   A version-controlled **`custom.el`** in this repository for personal
    settings that sync across machines (frame size, Magit shortcuts,
    Copilot, and more).

Installation
=================

If you previously had another version of Emacs installed it is a good
idea to move your `~/.emacs.d` configuration folder to a backup
location before installing this Emacs configuration. If you do not yet
have Emacs, installers available for [Mac OSX](https://emacsformacosx.com/) 
and [Windows](https://vigou3.gitlab.io/emacs-modified-windows/). 

Once Emacs is installed, install this configuration by copying the
files from https://github.com/iqss/IQSS.emacs to a folder named
`.emacs.d` in your home directory. For example, you can run `git clone
https://github.com/IQSS/IQSS.emacs.git ~/.emacs.d`, or you can download
the .zip archive from
https://github.com/IQSS/IQSS.emacs/archive/master.zip, extract it, and
move the files to `~/.emacs.d`.

**Updating an existing install** (e.g., on another computer): if
`~/.emacs.d` is already a clone of this repository, run `git pull` in
that directory before starting Emacs. Personal settings in `custom.el`
are tracked in git, so they follow the same workflow as `init.el`.

First run
=============

Note that after installing this configuration emacs will be slow to
start up the first time. This is due to package installation; just be
patient and wait for it to finish--subsequent start-ups will be much
faster.


Getting started
======================

If you have never used Emacs before many things will work as you
expect. This is especially true on Mac OS X. If you use Windows, note
that **standard Windows shortcuts starting with `control` have been
shifted to the `windows` key**. For example, to copy use `win-c`
rather than `control-c`, and to paste use `win-v` rather than `control-c`.

A few things may not work as you expect, in which case you will need
to search the web or read the Emacs documentation to learn the Emacs
way. You can launch a built-in tutorial by pressing =C-h t= (that's
"Control+h, then t"), or read the getting started documentation at
https://www.gnu.org/software/emacs/tour/. A cheat-sheet / survival
guide is available at
https://www.gnu.org/software/emacs/refcards/pdf/survival.pdf.

If you are an Emacs user, most things will mostly work as you expect,
though you may wish to familiarize yourself with the alternative key
bindings configured here. If you are an Emacs user and you find key
bindings that don't work as they should please open an issue in
the [[https://github.com/IQSS/IQSS.emacs][github repo]].


Keyboard shortcuts
==========================

This documentation mostly uses Emacs notation for keybindings, e.g.,
`C` means "the Control key", `S` means "the Shift key", and `M` means
"the Meta (aka Alt) key". Note that on a Mac `M` means "the Option
key". Refer to <https://www.emacswiki.org/emacs/EmacsKeyNotation> if
you are not familiar with this notation.

The most important keyboard shortcut in Emacs is `M-x` (that's "hold
down Alt and press x" Windows, and "hold down Option and press x" on
Mac). `M-x` brings up a search-able list of all Emacs commands. In fact
you could use this interface for everything and never bother learning
any of the other keybindings listed below. For example, to open a file
you could type `M-x counsel-find-file <ret>` instead of `win-o`. Nobody
does this in practice, because `win-o` is easier. But if you can't
remember the name of a keyboard shortcut don't worry: just type `M-x`
and search for the command you need.

The second most important keyboard shortcut is `C-g` (that's "hold
down control and press g"). If Emacs starts doing something you don't
want it to, press `C-g` to cancel. If it doesn't work, press `C-g`
again.

Other commonly used key bindings are listed in the following sections.

Standard shortcuts
----------------------

On Mac OS X standard keyboard shortcuts should mostly work as
expected.

On Windows, many common keyboard shortcuts start with the control key.
This is a problem for Emacs, since many of it's most-used shortcuts
conflict with the usual Windows meaning. For example, `C-a` means
"select all" on Windows, but "go to the beginning of the line" in
Emacs. The solution adopted here is to move standard Windows
keybindings to the `win` key. The advantage is that we retain normal
Emacs behavior (`C-a` has the Emacs meaning of "go to the beginnign of
the line). The disadvantage is that you'll have to get used to
pressing different keys (`win-a` instead of `c-a` to select all).


Multiple cursors
-----------------

You can add multiple cursors by pressing `C-c m` and following the
on-screen prompts. This feature is experimental; comments or
suggestions welcome at <https://github.com/IQSS/IQSS.emacs/issues>.
For more information about the Emacs multiple cursors implementation
refer to <https://github.com/magnars/multiple-cursors.el>.

Window management
----------------------

One of the things that makes Emacs different that most other
applications is the way that it handles windows. Unlike most Integrated
Development Environments, there is no fixed layout. Instead, windows are
created and killed as needed. New Emacs uses sometimes try to get Emacs
to stop messing with their window layout -- my approach is to just let
Emacs do what it wants and the revert the layout using `C-c left`.


  Key                     |Description                  
  ------------------------|-----------------------------
  `C-x 2`                 | Split horizontally          
  `C-x 3`                 | Split vertically            
  `C-x 1`                 | Remove splits               
  `C-x S-<arrow>`         | Move to other window        
  `C-x S-0`               | Move to a window by number  
  `C-c left`              | Undo a window layout change 
  `C-c right`             | Redo a window layout change 
  `C-c v`                 | Save window layout          
  `C-c V`                 | Restore a saved window layout
  `C-c a`                 | Rotate window arrangements
  `C-c b`                 | Rotate buffers

Version control (Magit)
-------------------------

[Magit](https://magit.vc/) is the Git interface used with this
configuration. After you `git pull` changes to a file, open the file
in Emacs and use Magit to see what changed.

  Key        | Description
  -----------|----------------------------------------------------------
  `C-x g`    | Open Magit status for the current Git repository
  `l`        | Log (in the status buffer); `l` again on a commit for details
  `d`        | Diff the change at point
  `q`        | Quit the Magit buffer

**Viewing changes to the file you are editing:** with the file open,
run `M-x magit-log-buffer-file` and press `d` on a commit to see its
diff. Word-level highlights inside diff hunks are enabled
(`magit-diff-refine-hunk`); press `h` in a diff buffer to toggle
refinement on a hunk.

**Viewing recent pulls:** `M-x magit-reflog-head` shows where `HEAD`
has been (including fast-forward pulls); `d` on an entry shows the
diff. For a single file, `M-x magit-log-buffer-file` lists commits that
touched that file.

Searching and Completion
------------------------------

Utilities have been configured to make it easy to search by file name as
well as to search the contents of files. Some of this functionality
works much better if certain system utilities are found. See [this list
of useful programs](https://IQSS.github.io/IQSS.emacs/UsefulPrograms.html), especially *everything*
(windows only) and *the silver searcher* or *ripgrep*.

Basic search/replace should work as you expect, except that again on
Windows you should use the `win` key instead of the `control` key. For
example, you can use `win-f` to search.

In addition, you can search for files in a directory by name or
contents using the keys described in the table below.

  Key        |Description                                 |Notes
  -----------|--------------------------------------------|------------------------------------------------------------------------------------
  `win-f`    |Find in file                                |
  `C-c l`    |Searches for files by name                  |(think "locates")
  `C-c f`    |(or `C-c s`) Searches file contents         |requires `mlocate` on linux, `everything` (<http://www.voidtools.com/>) on windows
  `<tab>`    |Completion suggestions                      |
  `win-S-v`  |Paste from the clipboard history            |`M-S-y` also works for this
  `C-c r`    |Search for a reference to insert            |You must set `bibtex-completion-bibliography` to your BibTeX files for this to work
  
REPL interaction
--------------------

This should be easy, and hopefully it is!

Note that we use a heuristic to decide whether to install language
support (e.g., for *R* or *Scala* etc.). If the corresponding program
is in your `PATH` Emacs support will be installed. For example, if `R`
is in your `PATH` the *ESS* package will be installed.

Aliases have been created for starting R, python, haskell, and
terminals. For example, to start python just type `M-x python <ret>`.

To execute a line, region, or buffer from a script (R, python, bash)
etc.) use the keybindings below.

  Key        |Description                           |Notes
  -----------|--------------------------------------|----------------------------------------
  `C-RET`    |Line/selection/expression evaluation  |Works for R, python, shell, and others
  `S-C-RET`  |Buffer evaluation                     |Evaluate the whole script


Interacting with external programs
----------------------------------

Many of the Emacs features configured here are designed to make it
easier to interact with external programs. For example,
[ESS](http://ess.r-project.org) makes it easy to interact with
[R](http://r-project.org), and
[AUCTEX](https://www.gnu.org/software/auctex/) makes it easy to interact
with [LaTeX](http://tug.org/texlive/). If you need help installing these
programs, [this short guide](http://iqss.github.io/IQSS.emacs/UsefulPrograms.html) 
may help.


Customization
=============

You can put any additional Emacs configuration in
`~/.emacs.d/custom.el`. This file is loaded last, so you always have
the chance to override any settings you don't like. **`custom.el` is
part of this git repository**, so edits you commit and push are available
on other machines after `git pull`. Machine-specific Customize UI
settings are stored separately in `custom-settings.el` (not tracked in
git).

You can require additional packages by adding them to
`package-selected-packages`. For example, putting `(add-to-list
'package-selected-packages 'matlab-mode)` in your `custom.el` file will
ensure that the *matlab-mode* package is installed.

Settings currently in the shared `custom.el`
----------------------------------------------

The following personal settings are documented here so you know what
ships with the repo and how to change them. All of this lives in
`custom.el`; adjust values there and commit if you want them everywhere.

| Setting | What it does |
|--------|----------------|
| **Font** | Default face: Monaco at height 180. |
| **Frame size** | On graphical displays, new frames open at 1053×1382 pixels when the monitor is large enough; on smaller screens (e.g. laptop only) the size is clamped to the work area. Frames are placed with their **right edge** aligned to the right of the screen. Still freely resizable. Implemented by `my/set-default-frame-size`. |
| **Scroll keys** | `C-up` / `C-down` scroll the view one line without moving point. |
| **Quit Emacs (macOS)** | `Cmd+Q` does not quit Emacs (shows a reminder); use `C-x C-c` to quit. |
| **Magit** | `C-x g` → `magit-status`; diffs use word-level refinement (`magit-diff-refine-hunk`). See [Version control](#version-control-magit). |
| **Copilot** | `copilot.el` via `quelpa`; accept completion with `M-C-return` or `C-right`. |
| **Quelpa / MELPA** | `quelpa-update-melpa-p` is `nil` so the MELPA recipe repo is not re-cloned on every Emacs startup. Set it to `t` or run `M-x quelpa-upgrade-all` when you want to refresh Copilot. |

**Copilot** requires network access the first time Emacs installs the
package. The bootstrap block at the top of `custom.el` still runs
`package-install-selected-packages` when needed; do not remove it.

**Frame dimensions** are the pixel size `1053` and `1382` in
`my/set-default-frame-size`; change those numbers in `custom.el` if you
want a different default on large monitors.


More information
=============

For more information refer to the [annotated configuration file](https://iqss.github.io/IQSS.emacs/init.html).
