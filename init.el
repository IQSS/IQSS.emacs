
;;; COMMENTARY

;; This emacs configuration file sets some convenient defaults and activates 
;; emacs functionality useful to social scientists. 


;; NOTE FOR RCE USERS: RCE Emacs has some strange system configuration
;; settings. To use this init file on the RCE you need to start emacs with
;; emacs --no-site-file --no-site-lisp. This is a temporary requirement that
;; will eventually be resolved in cooperation with the RCE team.

;; hide the toolbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;; Install required packages

;; load the package manager
(require 'package)

;; Add additional package sources
(add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Make a list of the packages you want
(setq package-list '(leuven-theme
                     persistent-soft
                     unicode-fonts
                     ido
                     ido-ubiquitous
                     ido-vertical-mode
                     noflet
                     kill-ring-ido
                     smex
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
                     slime
                     htmlize
                     pcmpl-args
                     pcmpl-pip
                     readline-complete))

;; Activate package autoloads
(package-initialize)

;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install packages in package-list if they are not already installed
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; finally a theme I can live with!
(load-theme 'leuven t) 
(setq org-fontify-whole-heading-line t)

;; add custom lisp directory to path
(let ((default-directory (concat user-emacs-directory "lisp/")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; enable on-the-fly spell checking
(add-hook 'after-init-hook
          (lambda ()
            (add-hook 'text-mode-hook
                      (lambda ()
                        (flyspell-mode 1)))))

;; prevent flyspell from finding mistakes in the code
(add-hook 'after-init-hook
          (lambda ()
            (add-hook 'prog-mode-hook
                      (lambda ()
                        ;; `ispell-comments-and-strings'
                        (flyspell-prog-mode)))))

(require 'persistent-soft)
(require 'unicode-fonts)
(unicode-fonts-setup)

;;; Completion hints for files and buffers buffers
(setq ido-file-extensions-order '(".r" ".sh" ".tex" ".bib" ".org" ".txt" ".html" 
                                  ".py" ".emacs" ".xml" ".el" ".pdf" 
                                  ".png" ".ini" ".cfg" ".conf"))

;; load ido 
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; use ido everywhere you can
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
 
;; present ido suggestions vertically
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; set nice ido decorations
(setq ido-decorations '("
▶ " "" "
   " "
   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "
-> " ""))

;; set sensible keys for id in vertical mode
(setq ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))

;; use ido for kill-ring
(require 'kill-ring-ido)
(global-set-key (kbd "M-y") 'kill-ring-ido)

;;; Completion hints for emacs functions
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; modify smex so that typing a space will insert a hyphen 
;; (from http://www.emacswiki.org/Smex#toc6)
    (defadvice smex (around space-inserts-hyphen activate compile)
      (let ((ido-cannot-complete-command 
             `(lambda ()
                (interactive)
                (if (string= " " (this-command-keys))
                    (insert ?-)
                  (funcall ,ido-cannot-complete-command)))))
        ad-do-it))

;;; Auto-complete

;; Set up autocomplete sources
(require 'auto-complete-config)
(ac-config-default)

;; use tab for completion; don't start automatically
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
;; similar thing, for company mode
;; from https://github.com/company-mode/company-mode/issues/94
(eval-after-load "company"
  '(progn
     (define-key company-mode-map [remap indent-for-tab-command]
       'company-indent-for-tab-command)
     (setq tab-always-indent 'complete)
     (defvar completion-at-point-functions-saved nil)
     (defun company-indent-for-tab-command (&optional arg)
       (interactive "P")
       (let ((completion-at-point-functions-saved completion-at-point-functions)
             (completion-at-point-functions '(company-complete-common-wrapper)))
         (indent-for-tab-command arg)))
     (defun company-complete-common-wrapper ()
       (let ((completion-at-point-functions completion-at-point-functions-saved))
         (company-complete-common)))
     (setq company-idle-delay nil)))
;; elpy tries to over-ride this--try to take the power back.
(add-hook 'elpy-mode-hook
          (lambda ()
            (setq company-idle-delay nil)))
;; disable ac-mode in python-mode because elpy uses company instead
;; workaround so auto-complete works with flyspell
(setq ac-modes (remove "python-mode" ac-modes))

;; required or auto-complete bogs down when flyspell is active
(ac-flyspell-workaround)

;;; Configure outline minor modes
;; Less crazy key bindings for outline-minor-mode
(setq outline-minor-mode-prefix "\C-c\C-o")
;; load outline-magic along with outline-minor-mode
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
            (define-key outline-minor-mode-map "\C-c\C-o\t" 'outline-cycle)))
;; turn on for some modes:
(add-hook 'LaTeX-mode-hook 'outline-minor-mode t)
(add-hook 'prog-mode-hook 'outline-minor-mode t)

;;; AucTeX config
;; turn on math mode and and index to imenu
(add-hook 'LaTeX-mode-hook 
          '(lambda ()
             (turn-on-reftex)
             (TeX-PDF-mode t)
             (LaTeX-math-mode)
             (imenu-add-to-menubar "Index")
;; Allow paragraph filling in tables
             (setq LaTeX-indent-environment-list
                   (delq (assoc "table" LaTeX-indent-environment-list)
                         LaTeX-indent-environment-list))
             (setq LaTeX-indent-environment-list
                   (delq (assoc "table*" LaTeX-indent-environment-list)
                         LaTeX-indent-environment-list))))
;; Misc. latex settings
(setq TeX-parse-self t
      TeX-auto-save t)
(setq-default TeX-master nil)
;; Add beamer frames to outline list
(setq TeX-outline-extra
      '(("\\\\begin{frame}\n\\|\\\\begin{frame}.*{.*}\\|[       ]*\\\\frametitle\\b" 3)))
;; reftex settings
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'bibtex-mode-hook
          '(lambda ()
             (define-key bibtex-mode-map "\M-q" 'bibtex-fill-entry)))

;;; markdown mode

;; Use markdown-mode for files with .markdown or .md extensions
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'org)

;; Load additional export formats
;; (require 'ox-odt)
;; (require 'ox-md)
;; (require 'ox-freemind)
;; (require 'ox-bibtex)

;; Update images from babel code blocks automatically
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Enable common programming language support in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (matlab . t)
   (emacs-lisp . t)
   (sh . t)
   (dot . t)
   ;; (latex . t)
   ;; (octave . t)
   ;; (ditaa . t)
   ;; (org . t)
   ;; (perl . t)
))

;; Set sensible mode for editing dot files
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Fontify code blocks in org-mode
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;;;  ESS (Emacs Speaks Statistics)

;; Start R in the working directory by default
(setq ess-ask-for-ess-directory nil)

;; Scroll down when R generates output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; Make sure ESS is loaded
(require 'ess-site)

;;; polymode

;; polymode requires emacs >= 24.3, does not work on the RCE. 
(when (>= (string-to-number 
           (concat 
            (number-to-string emacs-major-version) 
            "." 
            (number-to-string emacs-minor-version)))
          24.3)
  ;; Activate polymode for files with the .md extension
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  ;; Activate polymode for R related modes
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rapport" . poly-rapport-mode))
  (add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rbrew" . poly-brew+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode)))

;;; Misc. Conveniences

;; use regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Make sure copy-and-paste works with other programs
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Text pasted with mouse should be inserted at cursor position
(setq mouse-yank-at-point t)

;; Mouse scrolling behavior
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Put backups in a separate folder
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Apropos commands should search everything
(setq apropos-do-all t)

;; Store the places file in the emacs user directory
(setq save-place-file (concat user-emacs-directory "places"))


;; better naming of duplicate buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; put cursor in last used position when re-opening file
(require 'saveplace)
(setq-default save-place t)

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode 1) ; makes the region visible
(line-number-mode 1)    ; makes the line number show up
(column-number-mode 1)  ; makes the column number show up

(setq global-font-lock-mode 1) ; everything should use fonts
(setq font-lock-maximum-decoration t) ;; decorate as much as possible
(show-paren-mode t) ;; highlight matching paren

;; smooth scrolling with C-up/C-down
(require 'smooth-scroll)
(smooth-scroll-mode)
(global-set-key [(control down)] 'scroll-up-1)
(global-set-key [(control up)] 'scroll-down-1)
(global-set-key [(control left)] 'scroll-right-1)
(global-set-key [(control right)] 'scroll-left-1)

;; enable toggling paragraph fill
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal most-positive-fixnum) (deactivate-mark nil))
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))
      (if (use-region-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))))
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(global-set-key (kbd "M-q") 'compact-uncompact-block)

;; visual line mode
(global-visual-line-mode 1) 

;; don't require two spaces for sentence end.
(setq sentence-end-double-space nil)

;; Use CUA mode only for handy rectangle features
(cua-selection-mode t)

;; windmove is nice but hard to find free key-binding...
(windmove-default-keybindings 'super)

;; The beeping can be annoying--turn it off
(set-variable 'visible-bell t)

;; save settings made using the customize interface to a sparate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(byte-recompile-file custom-file nil 0 nil)
(load (concat user-emacs-directory "custom.elc") 'noerror)

;; byte-compile init file if needed
(add-hook 'after-init-hook
          (lambda ()
            (byte-recompile-file user-init-file nil 1 nil)
            (switch-to-buffer "*scratch*")
            (delete-other-windows)))
