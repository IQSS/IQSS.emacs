;;; eval-in-repl-ielm.el --- ESS-like eval to .el files and ielm  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Kazuki YOSHIDA

;; Author: Kazuki YOSHIDA <kazukiyoshida@mail.harvard.edu>
;; Keywords: tools, convenience
;; URL: https://github.com/kaz-yos/eval-in-repl
;; Version: 0.1.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; ielm-specific file for eval-in-repl
;; See below for configuration
;; https://github.com/kaz-yos/eval-in-repl/


;;; Code:

;;;
;;; Require dependencies
(require 'eval-in-repl)
(require 'ielm)


;;;
;;; EMACS LISP RELATED
;;; eir-send-to-ielm
(defun eir-send-to-ielm (start end)
  "Sends expression to *ielm* and have it evaluated."

  (eir-send-to-repl start end
		    ;; fun-change-to-repl
		    #'(lambda () (switch-to-buffer-other-window "*ielm*"))
		    ;; fun-execute
		    #'ielm-return))
;;
;;; eir-eval-in-ielm
;;;###autoload
(defun eir-eval-in-ielm ()
  "This is a customized version of eir-eval-in-repl-lisp for ielm."

  (interactive)
  (eir-eval-in-repl-lisp
   ;; repl-buffer-regexp
   "\\*ielm\\*"
   ;; fun-repl-start
   #'ielm
   ;; fun-repl-send
   #'eir-send-to-ielm
   ;; defun-string
   "(defun "))
;;

(provide 'eval-in-repl-ielm)
;;; eval-in-repl-ielm.el ends here

