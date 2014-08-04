(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(lpr-command "gtklp")
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Liberation Sans")))))


;;; other overrides go here

;; install other packages I like
(setq package-list '(smex
                     ido-ubiquitous
                     outline-magic
                     smooth-scroll
                     auto-complete
                     auctex
                     ess 
                     org-plus-contrib
                     markdown-mode 
                     polymode
                     eval-in-repl
                     elpy
                     cider
                     slime))

;; Activate package autoloads
(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install packages in package-list if they are not already installed
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;;; from http://www.emacswiki.org/emacs/essh.el

;;; essh.el --- a set of commands that emulate for bash what ESS is to R.

;; Filename: essh.el


;; ------------------------------------------------------------------ ;;
;; TO INSTALL:                                                        ;;
;; 1. add essh.el in your load-path.                                  ;;
;;                                                                    ;;
;; 2. add to your .emacs file:                                        ;;
;;                                                                    ;;
;; (require 'essh)                                                    ;;
;; (defun essh-sh-hook ()                                             ;;
;;   (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)        ;;
;;   (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)        ;;
;;   (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)          ;;
;;   (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step) ;;
;;   (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)      ;;
;;   (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)) ;;
;; (add-hook 'sh-mode-hook 'essh-sh-hook)                             ;;
;; ------------------------------------------------------------------ ;;

;; function taken from ess package
(defun essh-next-code-line (&optional arg)
  "Move ARG lines of code forward (backward if ARG is negative).
Skips past all empty and comment lines.	 Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc)); n=0 is success
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun process-shell ()
  "returns a list with existing shell process."
  (interactive)
  (setq listpr (process-list))
  (setq lengthpr (length listpr))
  (setq i 0)
  (setq listshellp '())
  (while (< i lengthpr)
    (setq pos (string-match "shell" (prin1-to-string (elt listpr i))))
    (if pos (add-to-list 'listshellp (process-name (get-process (elt listpr i)))))
    (setq i (+ 1 i)))
  listshellp)


(defun process-shell-choose ()
  "returns which process to use."
(interactive)
(setq outpr 0)
(setq cbuf (current-buffer))
(setq shelllist (process-shell))
(setq shelln (length shelllist))
(if (eq shelln 0)
    (progn (shell)
	   (switch-to-buffer cbuf)
	   (setq outpr (get-process "shell"))
	   (sleep-for 0.5)))
(if (eq shelln 1)
    (setq outpr (get-process (elt shelllist 0))))
(if (> shelln 1)
(progn
(setq proc (completing-read "Send code to:" shelllist nil t (elt shelllist 0)))
(setq outpr (get-process proc))))
outpr)


(defun shell-eval-line (sprocess command)
  "Evaluates a single command into the shell process."
  (setq sbuffer (process-buffer sprocess))
  (setq command (concat command "\n"))
  (accept-process-output sprocess 0 10)
  (with-current-buffer sbuffer 
    (end-of-buffer) ;point is not seen being moved (unless sbuffer is focused)
    (insert command)			;pastes the command to shell
    (set-marker (process-mark sprocess) (point-max))
    (process-send-string sprocess command)
    ;; (accept-process-output sprocess 0 10)
    ))

(defun shell-cd-current-directory ()
  "Changes the shell working directory to the current buffer's one."
  (interactive)
  (setq sprocess (process-shell-choose))
  (setq com (format "cd %s" (file-name-directory default-directory)))
  (shell-eval-line sprocess com))


(defun pipe-line-to-shell (&optional step)
  "Evaluates the current line to the shell."
  (interactive ())
  (setq com (buffer-substring (point-at-bol) (point-at-eol)))
  (if (> (length com) 0)
      (progn
	(setq sprocess (process-shell-choose))
	(shell-eval-line sprocess com)
	(when step (essh-next-code-line)))
    (message "No command in this line")))

(defun pipe-line-to-shell-and-step ()
  "Evaluates the current line to the shell and goes to next line."
  (interactive)
  (pipe-line-to-shell t))

(defun pipe-region-to-shell (start end)
  "Sends a region to the shell."
  (interactive "r")
  (setq com (buffer-substring start end))	       ;reads command
  (setq lcom (length com))		       ;count chars
  (setq lastchar (substring com (1- lcom) lcom)) ;get last char
  (unless (string-match "\n" lastchar) ;if last char is not "\n", then...
    (setq com (concat com "\n")))	     ;...add it!
  (setq sprocess (process-shell-choose))
  (setq sbuffer (process-buffer sprocess))
  (while (> (length com) 0) 
    (setq pos (string-match "\n" com)) 
    (setq scom (substring com 0 pos))
    (setq com (substring com (min (length com) (1+ pos))))
    (shell-eval-line sprocess scom)
    (accept-process-output sprocess 0 10)
    )) 


(defun pipe-buffer-to-shell ()
  "Evaluate whole buffer to the shell."
  (interactive)
  (pipe-region-to-shell (point-min) (point-max)))

(defun pipe-function-to-shell ()
"Evaluate function to the shell."
(interactive)
(setq beg-end (essh-beg-end-of-function))
(if beg-end
    (save-excursion
      (setq beg (nth 0 beg-end))
      (setq end (nth 1 beg-end))
      (goto-line beg)
      (setq origin (point-at-bol))
      (goto-line end)
      (setq terminal (point-at-eol))
      (pipe-region-to-shell origin terminal))
  (message "No function at current point.")))

(defun essh-beg-end-of-function ()
  "Returns the lines where the function starts and ends. If there is no function at current line, it returns nil."
  (interactive)
  (setq curline (line-number-at-pos))	;current line
  (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
  (setq pos (string-match "function" curcom))
  (save-excursion 
    (if pos 
	(progn
	  (setq beg curline))
      (progn
	(while (not pos)
	  (setq curline (1- curline))	;current line
	  (previous-line)			;go to previous line
	  (setq curcom (buffer-substring (point-at-bol) (point-at-eol)))
	  (setq pos (string-match "function" curcom)))
      (setq beg curline)))
    (beginning-of-line)
    (forward-list)			; move pointer to first matching brace
    (setq end (line-number-at-pos)))
  ;; (message (format  "%d %d" beg end))
  (if (and (<= (line-number-at-pos) end) (>= (line-number-at-pos) beg))
      (list beg end)
    nil))
  

(provide 'essh)

;; Python completion and code checking
(elpy-enable)


;; window arrangement history
(winner-mode 1)

;; printing
(setq ps-lpr-command "gtklp")
;; ;; Look recursively in .emacs.d/lisp for elisp files and packages
(let ((default-directory "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(require 'ob-stata)
(require 'org-ref)

;; require the main file containing common functions
(require 'eval-in-repl)

;; ielm
(require 'eval-in-repl-ielm)
;; For .el files
(define-key emacs-lisp-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
;; For *scratch*
(define-key lisp-interaction-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
;; For M-x info
(define-key Info-mode-map "\C-c\C-c" 'eir-eval-in-ielm)

;; cider
(require 'cider) ; if not done elsewhere
(require 'eval-in-repl-cider)
;; (define-key clojure-mode-map "\C-c\C-c" 'eir-eval-in-cider)

;; SLIME
(require 'slime) ; if not done elsewhere
(require 'eval-in-repl-slime)
(add-hook 'lisp-mode-hook
		  '(lambda ()
		     (local-set-key "\C-c\C-c" 'eir-eval-in-slime)))

;; scheme
(require 'scheme) ; if not done elsewhere
(require 'cmuscheme) ; if not done elsewhere
(require 'eval-in-repl-scheme)
(add-hook 'scheme-mode-hook
		  '(lambda ()
		     (local-set-key "\C-c\C-c" 'eir-eval-in-scheme)))

;; python
(require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(add-hook 'elpy-mode-hook
		  '(lambda ()
                     (define-key elpy-mode-map "\C-c\C-c" 'eir-eval-in-python)))

;; shell
(require 'essh) ; if not done elsewhere
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
		     (local-set-key "\C-c\C-c" 'eir-eval-in-shell)))

(require 'org-capture)

(require 'org-protocol)

;; set up capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/notes.org" "RT Tasks")
         "* TODO %?\n  %i\n  %a")))

(define-key global-map "\C-cc" 'org-capture)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (matlab . t)
   (emacs-lisp . t)
   (stata . t)
   (sh . t)
   ;; (dot . t)
   ;; (latex . t)
   ;; (octave . t)
   ;; (ditaa . t)
   (org . t)
   ;; (perl . t)
))

(setq org-confirm-babel-evaluate nil)

;;; email and stuff (I really think the browser is better...)
;; (require 'rt-liberation)

;; ;; Tell rt-liberation where to find the RT binary and which version is
;; ;; being used, for example:

;; (setq rt-liber-rt-binary "/opt/rt4/bin/rt"
;;       rt-liber-rt-version "4.0.15")

;; ;; rt-liberation can issue a command to ``take'' a ticket, that is,
;; ;; assign it to yourself. For this the variable @var{rt-liber-username}
;; ;; must be set:

;; (setq rt-liber-username "izahn")

;; ;; rt-liberation can also launch a Web browser to visit a ticket. For
;; ;; that to work the base URL needs to be set in
;; ;; @var{rt-liber-base-url}. For example:

;; (setq rt-liber-base-url "https://help.hmdc.harvard.edu/")


;; ;; email 
;; (require 'mu4e)

;; ;; default
;; (setq mu4e-maildir "~/Maildir")

;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; (setq mu4e-trash-folder  "/[Gmail].Trash")

;; ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; ;; setup some handy shortcuts
;; ;; you can quickly switch to your Inbox -- press ``ji''
;; ;; then, when you want archive some messages, move them to
;; ;; the 'All Mail' folder by pressing ``ma''.

;; (setq mu4e-maildir-shortcuts
;;     '( ("/INBOX"               . ?i)
;;        ("/[Gmail].Sent Mail"   . ?s)
;;        ("/[Gmail].Trash"       . ?t)))

;; ;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")

;; ;; something about ourselves
;; (setq
;;    user-mail-address "istazahn@gmail.com"
;;    user-full-name  "Ista Zahn"
;;    mu4e-compose-signature
;;     (concat
;;       "Best,"
;;       "Ista"))

;; ;; sending mail -- replace USERNAME with your gmail username
;; ;; also, make sure the gnutls command line utils are installed
;; ;; package 'gnutls-bin' in Debian/Ubuntu

;; (require 'smtpmail)
;; ;; (setq message-send-mail-function 'smtpmail-send-it
;; ;;    starttls-use-gnutls t
;; ;;    smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;; ;;    smtpmail-auth-credentials
;; ;;      '(("smtp.gmail.com" 587 "USERNAME@gmail.com" nil))
;; ;;    smtpmail-default-smtp-server "smtp.gmail.com"
;; ;;    smtpmail-smtp-server "smtp.gmail.com"
;; ;;    smtpmail-smtp-service 587)

;; ;; alternatively, for emacs-24 you can use:
;; (setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; ;; don't keep message buffers around
;; (setq message-kill-buffer-on-exit t)

;; ;; use 'fancy' non-ascii characters in various places in mu4e
;; (setq mu4e-use-fancy-chars t)

;; ;; save attachment to my desktop (this can also be a function)
;; (setq mu4e-attachment-dir "~/Downloads")

;; ;; Add missing folder overview functionality
;; ;(require 'mu4e-maildirs-extension)
;; ;(mu4e-maildirs-extension)

;; ;; conversation view
;; (setq mu4e-headers-include-related t)

;; ;; don't need to see that many...
;; (setq mu4e-headers-results-limit 100)
