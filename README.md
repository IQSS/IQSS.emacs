Emacs for the (social science) masses
=====================================

This is an [Emacs](https://www.gnu.org/software/emacs/) configuration.
There are many like it, but this one is mine. If you like it, make it
yours! It provides lots of functionality while keeping things light
and fast.

Project goals and philosophy
----------------------------

The main goal of this project is to provide an Emacs configuration
that works more or less they way you would expect an editor or IDE to
work in the second decade of the twenty-first century, without losing
the things that make Emacs special. This is challenging because basic
Emacs commands often conflict with de facto standards. Each of these
conflicts is a judgment call; hopefully a good balance has been
reached. The overarching philosophy is pragmatism; we're trying to
make Emacs as useful as possible.

Feature highlights
------------------

Highlights of this Emacs configuration include:
- More standard select/copy/paste keys and right-click behavior makes
  it more familiar to those new to Emacs.
- Consistent and familiar indentation *and* code completion using the
  `tab` key.
- Consistent and familiar code evaluation using `C-ret`.
- Literate programming configuration for running R, python, or other
  programming languages inside markdown or org-mode files.
- Powerful and simple search-based tools for finding commands, files
  and buffers, inserting citations etc.
- Convenient window management

A note for Mac users
--------------------

The keybindings configured here use Windows-style. I wish I could make
life easy for you by providing Mac style keyboard shortcuts, but that
is simply too much work to be feasible. Basically this mostly boils
down to "use the control key instead of the command key". For example,
use "Control-q" to quit instead of "Command-q" as you do with other
applications running on OS X.

Quick start
===========

How to I install it?
--------------------

1.  Make sure emacs &gt;= version 25.1 is installed on your computer.
    See [this list of useful programs](http://izahn.github.io/dotemacs/UsefulPrograms.html) 
    for installation instructions.
2.  Make sure you have [git](http://git-scm.com/downloads) installed on
    your computer. If you don't know what git is you might be interested
    in John McDonnell's 
    [git tutorial](http://nyuccl.org/pages/GitTutorial/).
3.  Determine your emacs configuration directory. Open emacs and type
    `C-x d ~/ RET`. This should open a directory listing buffer. Note
    the path at the very top of this file. This is were your `.emacs.d` 
    configuration directory should go.
4.  Close Emacs.
5.  Back up your existing `~/.emacs` file and `~/.emacs.d` directory
    (e.g., rename `.emacs` to `OLD.emacs` and rename `.emacs.d` to
    `OLD.emacs.d`).
6.  Clone this repository into `~/.emacs.d` by opening a terminal and
    running `git clone http://github.com/izahn/dotemacs ~/.emacs.d`.

If you don't know how to use git, you can skip steps 2 and 6 and simply
[download the files as a zip archive](https://github.com/izahn/dotemacs/archive/master.zip),
extract them, and move them into your .emacs.d directory instead.

First run
---------

Note that after installing this configuration emacs will be slow to
start up the first time. This is due to package installation; just be
patient and wait for it to finish--subsequent start-ups will be much
faster.

Important key bindings
----------------------

This configuration loads a lot of useful emacs packages, many of which
add key bindings. Documenting them all here would be too much (see the
documentation for each package if you need the details), so this section
describes only most important ones.

Note that this documentation mostly uses Emacs notation for
keybindings, e.g., `C` means "the Control key", `S` means "the Shift
key", and `M` means "the Meta (aka Alt) key". Note that on a Mac `M`
means "the Option key". Refer to
<https://www.emacswiki.org/emacs/EmacsKeyNotation> if you are not
familiar with this notation.

### Common keyboard shortcuts

The most important keyboard shortcut in Emacs is `M-x`. This brings up a
search-able list of all Emacs commands. In fact you could use this
interface for everything and never bother learning any of the other
keybindings listed below. For example, to open a file you could type
`M-x counsel-find-file <ret>` instead of `C-o`. Nobody does this in
practice, because `C=o` is easier. But if you can't remember the name of
a keyboard shortcut don't worry: just type `M-x` and search for the
command you need.

Other commonly used key bindings are listed in the table below.

  Key        |Description          |Notes
  -----------|---------------------|-------------------------------------------------------------------------
  `C-o`      |Open file            |
  `C-w`      |Close window         |
  `C-q`      |Quit                 |
  `S-arrow`  |Select a region      |`C-SPC arrow` does the same thing. `C-S-SPC` selects rectangular region
  `C-c`      |Copy selection       |
  `C-v`      |Paste                |
  `C-z`      |Undo                 |use `C-x U` or `M-z` to visualize your undo/redo history
  `S-C-z`    |Redo                 |
  `C--`      |Zoom out             |
  `C-+`      |Zoom in              |
  `C-PgUp`   |Beginning of buffer  |
  `C-PgDn`   |End of buffer        |

Note that some things still work "the Emacs way". Notably:

C-a
:   Goes to the beginning of the line. To select all use `C-x h`.

C-s
:   Searches. To save, use `C-x s`.

C-f
:   Moves forward one character. To search use `C-s`.

### Window management

One of the things that makes Emacs different that most other
applications is the way that it handles windows. Unlike most Integrated
Development Environments, there is no fixed layout. Instead, windows are
created and killed as needed. New Emacs uses sometimes try to get Emacs
to stop messing with their window layout -- my approach is to just let
Emacs do what it wants and the revert the layout using `C-c left`.

Some other convenient window management keys are provided, in addition
to the standard Emacs `C-x o` binding to navigate to "other window".

  Key                     |Description                  |Notes
  ------------------------|-----------------------------|--------------------------------------------------------------------------------
  `C-x S-<arrow>`         | Move to other window        | 
  `C-x S-0`               | Move to a window by number  | 
  `C-c left`              | Undo a window layout change | 
  `C-c right`             | Redo a window layout change | 
  `C-c C-l <number>`      | Save/restore window layouts | This a somewhat advanced feature that lets you save and restore window layouts
  
### Searching and Completion

Utilities have been configured to make it easy to search by file name as
well as to search the contents of files. Some of this functionality
works much better if certain system utilities are found. See [this list
of useful programs](UsefulPrograms.html), especially *everything*
(windows only) and *the silver searcher* or *ripgrep*.

  Key        |Description                                 |Notes
  -----------|--------------------------------------------|------------------------------------------------------------------------------------
  `C-s`      |Searches the current buffer using `swiper`  |
  `C-S-s`    |Searches files in the current directory     |
  `C-x S-f`  |(or `C-x O`) Searches by file name          |requires `mlocate` on linux, `everything` (<http://www.voidtools.com/>) on windows
  
Many standard Emacs keybindings have been replaced with versions that
provide completion suggestions. In-buffer completion can be triggered
with the `tab` key.

  Key    |Description                       |Notes
  -------|----------------------------------|-------------------------------------------------------------------------------------
  `TAB`    |Indent or complete                |
  `S-C-v`  |Paste from the clipboard history  |`M-S-y` also works for this
  `C-c r`  |Search for a reference to insert  |You must set `bibtex-completion-bibliography` to your BibTeX files for this to work
  
### REPL interaction

This should be easy, and hopefully it is!

Aliases have been created for starting R, python, haskell, and
terminals. For example, to start python just type `M-x python <ret>`.

To execute a line, region, or buffer from a script (R, python, bash)
etc.) use the keybindings below.

  Key        |Description                           |Notes
  -----------|--------------------------------------|----------------------------------------
  `C-RET`    |Line/selection/expression evaluation  |Works for R, python, shell, and others
  `S-C-RET`  |Buffer evaluation                     |Evaluate the whole script

### Other key bindings

There are a few more odds-and-ends you might find useful:

S-C-SPC
:   Edit rectangular regions

C-up / C-down
:   Scroll up or down

M-q
:   Hard-wrap a paragraph

M-S-Q
:   Remove line breaks from a paragraph

C-c C-o t
:   Hide/show outline (outline-minor mode is enabled in programming
    modes and in LaTeX-mode)

C-x cl
:   Echo keybindings for tutorials.

C-c d
:   Lookup word in dictionary.

Other key bindings can be discovered by `counsel-descbinds` (bound to
`C-h b`) or via the menus.

Interacting with external programs
----------------------------------

Many of the Emacs features configured here are designed to make it
easier to interact with external programs. For example,
[ESS](http://ess.r-project.org) makes it easy to interact with
[R](http://r-project.org), and
[AUCTEX](https://www.gnu.org/software/auctex/) makes it easy to interact
with [LaTeX](http://tug.org/texlive/). If you need help installing these
programs, [this short guide](http://izahn.github.io/dotemacs/UsefulPrograms.html) 
may help.

Documentation
=============

For more information refer to the [documentation](https://izahn.github.io/dotemacs).
