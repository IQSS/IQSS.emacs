Emacs for the (social science) masses
=====================================

This is an [https://www.gnu.org/software/emacs/](Emacs) configuration. There are many like it, but this one is mine. If you like it, make it yours! It provides lots of functionality while keeping things light and fast.

Project goals and philosophy
----------------------------

The main goal of this project is to provide an Emacs configuration that works more or less they way you would expect an editor or IDE to work in the second decade of the twenty-first century, without losing the things that make Emacs special. This is challenging because basic Emacs commands often conflict with de facto standards. Each of these conflicts is a judgment call; hopefully a good balance has been reached. The overarching philosophy is pragmatism; we're trying to make Emacs as useful as possible.

Feature highlights
------------------

Highlights of this Emacs configuration include:
- More standard select/copy/paste keys and right-click behavior makes it more familiar to those new to Emacs.
- Consistent use of the =shift= key to for related functionality. For example:
  - =C-z= is undo, =C-Z= is redo
  - =M-q= is hard warp, =M-Q= is unwrap
  - =C-s= searches in current buffer, =C=S= searches all buffers in the directory
  - =C-x f= (or =C-o=) opens files in the current directory, =C-x F= (or =C-O=) searches for files to open
- Consistent and familiar code completion using the =tab= key.
- Consistent and familiar code evaluation using =C-ret=.
- IDE-like configuration for R, Python, and LaTeX coding.
- Literate programming configuration for running R, python, or other programming languages inside markdown or org-mode files.
- Powerful and simple search-based tools for finding commands, files and buffers, inserting citations etc.
- Convenient window management, including navigation with =C-x O=, and undo/redo with =C-c left= and =C-c right=.
- Dictionary-lookup using =C-c d=.

For more information refer to the [https://izahn.github.io/dotemacs](documentation).
