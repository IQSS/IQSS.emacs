;;; other overrides go here
;;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)                      
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(add-hook 'eshell-first-time-mode-hook
		  '(lambda ()
		     (add-to-list 'eshell-visual-commands "nano")))



;; start the server if not already started
(load "server")
(unless (server-running-p) (server-start))

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
;(require 'org-ref)

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
(require 'essh) ; if not done elsewhere; essh is in the local lisp folder
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
		     (local-set-key "\C-c\C-c" 'eir-eval-in-shell)))

;; extra ESS stuff from https://github.com/gaborcsardi/dot-emacs/blob/master/.emacs
(ess-toggle-underscore nil)
(defun my-ess-post-run-hook ()
(ess-execute-screen-options)
(local-set-key "\C-cw" 'ess-execute-screen-options))
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)

;; auto-complete for shell and eshell
(require 'ac-pcomplete)
(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
(require 'readline-complete-autoloads)
(require 'readline-complete)
(add-to-list 'ac-modes 'eshell-mode)
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-process-echoes t)
;(add-to-list 'ac-modes 'shell-mode)
;(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))

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
   (latex . t)
   ;; (dot . t)
   ;; (latex . t)
   ;; (octave . t)
   ;; (ditaa . t)
   (org . t)
   ;; (perl . t)
))

(setq org-confirm-babel-evaluate nil)

;; highlight babel code block execution
(defadvice org-babel-execute-src-block (around progress nil activate)
  "create a buffer indicating what is running"
  (let ((ol (make-overlay (org-element-property :begin (org-element-at-point))
                          (org-element-property :end (org-element-at-point)))))
    (overlay-put ol 'face '(foreground-color . "blue"))
    ad-do-it
    (delete-overlay ol)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-cmpl-autolist t)
 '(term-buffer-maximum-size 1024)
 '(term-completion-autolist t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ; '(helm-selection ((t (:background "yellow"))))
)
