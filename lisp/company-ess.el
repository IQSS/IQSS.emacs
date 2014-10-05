;;; company-ESS.el --- R Completion Backend for Company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  

;; Author:  <Lompik@ORION>
;; Keywords: extensions, matching

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

;; 

;;; Code:



(require 'cl-lib)
(require 'company)
(require 'ess)


(defun ess-R-my-get-rcompletions (symb)
  "Call R internal completion utilities (rcomp) for possible completions.
"
  (let* (
	 
         ;; (opts1 (if no-args "op<-rc.options(args=FALSE)" ""))
         ;; (opts2 (if no-args "rc.options(op)" ""))
         (comm (format ".ess_get_completions(\"%s\", %d)\n"
		       (ess-quote-special-chars symb)
		       (length symb))))
    (ess-get-words-from-vector comm)))

(defun ess-company-args (symb)
  "Get the args of the function when inside parentheses."
  (when  ess--funname.start ;; stored by a coll to ess-ac-start-args
    (let ((args (nth 2 (ess-function-arguments (car ess--funname.start))))
          (len (length symb)))    
      (delete "..." args)
      (mapcar (lambda (a) (concat a ess-ac-R-argument-suffix))
              args))))


(defun ess-company-candidates ( symb)
  (let ((args (ess-company-args symb))
	(comps (cdr (ess-R-my-get-rcompletions symb))))
    
    (if args
	(setq comps (append
		     (delq nil (mapcar (lambda (x)
					 (if (string-match symb x)
					     x)) args))
		     comps)))
    comps))

(defun ess-company-start-args () ;SAme as ess-ac-start-args
  "Get initial position for args completion"
  (when (and ess-local-process-name
             (not (eq (get-text-property (point) 'face) 'font-lock-string-face)))
    (when (ess--funname.start)
      (if (looking-back "[(,]+[ \t\n]*")
          (point)
        (ess-symbol-start)))))


(defun ess-company-start ()
  (when (and ess-local-process-name
             (get-process ess-local-process-name))
					;(buffer-substring-no-properties (ess-ac-start) (point))
    (let ((start (or (ess-company-start-args)  (ess-symbol-start))))
      (when start
	(buffer-substring-no-properties start (point))))))

					;(company-grab-symbol)

(defun ess-R-get-typeof (symb)
  "Call R internal completion utilities (typeof) for possible completions.
"
  (let* ( ;; (opts1 (if no-args "op<-rc.options(args=FALSE)" ""))
         ;; (opts2 (if no-args "rc.options(op)" ""))
         (comm (format "typeof(%s)\n"
		       symb)))
    (format " %.3s" (car (ess-get-words-from-vector comm)))))

(defun ess-company-create-doc-buffer (syms)
  (let ((doc (ess-ac-help syms)))
    (company-doc-buffer doc)))


(defun company-ess-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'company-ess-backend))
    (prefix (ess-company-start))
    (candidates (ess-company-candidates arg))
    (doc-buffer (ess-company-create-doc-buffer arg))
    ;(meta (funcall ess-eldoc-function) )
    ;(annotation (ess-R-get-typeof arg))
    (sorted t) ; get arguments on top of the list
    (duplicates nil)
    ))

;(add-hook 'ess-mode-hook (lambda ()
;                          (set (make-local-variable 'company-backends) '(company-ess))
;                          (company-mode)))

(add-to-list 'company-backends 'company-ess-backend)

;(remove-hook 'completion-at-point-functions 'ess-R-object-completion) 
; FIXME: Is this required ?


(provide 'company-ess)
;;; company-ESS.el ends here
