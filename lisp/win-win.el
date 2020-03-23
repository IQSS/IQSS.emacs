;;; win-win.el --- Standard keybindings using the win key instead of control  -*- lexical-binding: t -*-

;; This file is modified from ns-win.el, with changes and additions
;; by Ista Zahn.
;; The original copyright notice is retained below.

;; Copyright (C) 1993-1994, 2005-2016 Free Software Foundation, Inc.

;; Authors: Carl Edman
;;	Christian Limpach
;;	Scott Bender
;;	Christophe de Dinechin
;;	Adrian Robert
;;      Ista Zahn
;; Keywords: terminals

;; This file is not part of GNU Emacs
;; (it is a modified version of a ns-win.el, which _is_ part of Gnu Emacs).

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Using Emacs on the mac is a much better experience since Emacs keys
;; don't conflict as much as they do with Windows keys. This file tries
;; to get Mac-style goodness on Windows as well.

;;; Code:
(eval-when-compile (require 'cl-lib))


;; Here are some Nextstep-like bindings for command key sequences.
(when (memq window-system '(w32))
  (w32-register-hot-key [?\s-,])
  (w32-register-hot-key [?\s-'])
  (w32-register-hot-key [?\s-`])
  (w32-register-hot-key [?\s--])
  (w32-register-hot-key [?\s-:])
  (w32-register-hot-key [?\s-?])
  (w32-register-hot-key [?\s-^])
  (w32-register-hot-key [?\s-&])
  (w32-register-hot-key [?\s-D])
  (w32-register-hot-key [?\s-E])
  (w32-register-hot-key [?\s-H])
  (w32-register-hot-key [?\s-L])
  (w32-register-hot-key [?\s-M])
  (w32-register-hot-key [?\s-S])
  (w32-register-hot-key [?\s-a])
  (w32-register-hot-key [?\s-c])
  (w32-register-hot-key [?\s-d])
  (w32-register-hot-key [?\s-f])
  (w32-register-hot-key [?\s-g])
  (w32-register-hot-key [?\s-h])
  (w32-register-hot-key [?\s-j])
  (w32-register-hot-key [?\s-k])
  (w32-register-hot-key [?\s-l])
  (w32-register-hot-key [?\s-m])
  (w32-register-hot-key [?\s-n])
  (w32-register-hot-key [?\s-o])
  (w32-register-hot-key [?\s-p])
  (w32-register-hot-key [?\s-q])
  (w32-register-hot-key [?\s-s])
  (w32-register-hot-key [?\s-u])
  (w32-register-hot-key [?\s-v])
  (w32-register-hot-key [?\s-w])
  (w32-register-hot-key [?\s-x])
  (w32-register-hot-key [?\s-y])
  (w32-register-hot-key [?\s-z])
  (w32-register-hot-key [?\s-Z])
  (w32-register-hot-key [?\s-|])
  (w32-register-hot-key [s-kp-bar]))

(define-key global-map [?\s-,] 'customize)
(define-key global-map [?\s-'] 'next-multiframe-window)
(define-key global-map [?\s-`] 'other-frame)
(define-key global-map [?\s--] 'center-line)
(define-key global-map [?\s-:] 'ispell)
(define-key global-map [?\s-?] 'info)
(define-key global-map [?\s-^] 'kill-some-buffers)
(define-key global-map [?\s-&] 'kill-this-buffer)
(define-key global-map [?\s-D] 'dired)
(define-key global-map [?\s-E] 'edit-abbrevs)
(define-key global-map [?\s-H] 'vr/replace)
(define-key global-map [?\s-L] 'shell-command)
(define-key global-map [?\s-M] 'manual-entry)
(define-key global-map [?\s-S] 'write-file)
(define-key global-map [?\s-a] 'mark-whole-buffer)
(define-key global-map [?\s-c] 'cua-copy-region)
(define-key global-map [?\s-d] 'isearch-forward-regexp)
(define-key global-map [?\s-f] 'isearch-forward-regexp)
(define-key global-map [?\s-h] 'vr/query-replace)
(define-key global-map [?\s-j] 'exchange-point-and-mark)
(define-key global-map [?\s-k] 'kill-this-buffer)
(define-key global-map [?\s-l] 'goto-line)
(define-key global-map [?\s-m] 'iconify-frame)
(define-key global-map [?\s-n] 'untitled-new-buffer-with-select-major-mode)
(define-key global-map [?\s-o] 'counsel-find-file)
(define-key global-map [?\s-p] 'hfyview-buffer)
(define-key global-map [?\s-q] 'save-buffers-kill-emacs)
(define-key global-map [?\s-s] 'save-buffer)
(define-key global-map [?\s-u] 'revert-buffer)
(define-key global-map [?\s-v] 'cua-paste)
(define-key global-map [?\s-w] 'kill-buffer-and-window)
(define-key global-map [?\s-x] 'cua-cut-region)
(define-key global-map [?\s-y] 'counsel-yank-pop)
(define-key global-map [?\s-z] 'undo)
(define-key global-map [?\s-Z] 'undo-tree-redo)
(define-key global-map [?\s-|] 'shell-command-on-region)
(define-key global-map [s-kp-bar] 'shell-command-on-region)
(define-key global-map (kbd "<M-kp-backspace>") 'kill-word)

;;;;;;;; Additional key-binding goodness from castlemacs ;;;;;;;;;;;;;;;
;; Use ESC as universal get me out of here command
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))



(provide 'win-win)

;;; win-win.el ends here
