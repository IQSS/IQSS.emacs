
;;; COMMENTARY

;; This emacs configuration file sets some convenient defaults and activates 
;; emacs functionality useful to social scientists. 


;; NOTE FOR RCE USERS: RCE Emacs has some strange system configuration
;; settings. To use this init file on the RCE you need to start emacs with
;; emacs --no-site-file --no-site-lisp. This is a temporary requirement that
;; will eventually be resolved in cooperation with the RCE team.

(when (< (string-to-number 
           (concat 
            (number-to-string emacs-major-version) 
            "." 
            (number-to-string emacs-minor-version)))
          24.2)
  (error "Your version of emacs is very old and must be upgraded before you can use these packages"))

;; hide the toolbar
(tool-bar-mode 0)
; (menu-bar-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;; Install required packages
(require 'cl)

;; load site-start early so we can override it later
(load "default" t t)
;; prevent site-start from running again later
(setq inhibit-default-init t)

;; load the package manager
(require 'package)

;; Add additional package sources
(add-to-list 'package-archives 
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Make a list of the packages you want
(setq my-package-list '(;; gnu packages
                        auctex
                        ;; melpa packages
                        leuven-theme
                        powerline
                        persistent-soft
                        unicode-fonts
                        dired+
                        mouse3
                        ido-ubiquitous
                        ido-vertical-mode
                        noflet
                        kill-ring-ido
                        smex
                        outline-magic
                        smooth-scroll
                        company
                        company-math
                        ess
                        markdown-mode
                        polymode
                        eval-in-repl
                        elpy
                        htmlize
                        pcmpl-args
                        pcmpl-pip
                        readline-complete
                        magit
                        ;; org-mode packages
                        org-plus-contrib))

;; Activate package autoloads
(package-initialize)

;; make sure stale packages don't get loaded
(dolist (package my-package-list)
  (if (featurep package)
      (unload-feature package t)))
;; Install packages in package-list if they are not already installed
(unless (every #'package-installed-p my-package-list)
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (setq my-this-buffer (buffer-name))
  (delete-other-windows)
  (insert "Please wait while emacs configures itself...")
  (redisplay t)
  (redisplay t)
  (package-refresh-contents)
  (dolist (package my-package-list)
    (when (not (package-installed-p package))
      (package-install package)))
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (delete-other-windows)
  (insert 
   ";; Your emacs has been configured for maximum productivity. 
;; For best results please restart emacs now.

;; More information about this emacs configuration be found
;; at http://github.com/izahn/dotemacs. If you have any problems
;; or have a feature request please open a bug report at
;; http://github.com/izahn/dotemacs/issues
"
   ))

;; finally a theme I can live with!
(load-theme 'leuven t) 
(setq org-fontify-whole-heading-line t)
(require 'powerline)
(powerline-default-theme)
(powerline-default-theme)

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

;; unicode-fonts doesn't work well on emacs < 24.3
(when (>= (string-to-number 
             (concat 
              (number-to-string emacs-major-version) 
              "." 
              (number-to-string emacs-minor-version)))
            24.3)
  (require 'persistent-soft)
  (require 'unicode-fonts)
  (unicode-fonts-setup))

;;; Completion hints for files and buffers buffers
(setq ido-file-extensions-order '(".R" ".r" ".sh" ".tex" ".bib" ".org" 
                                  ".py" ".emacs" ".xml" "org.el" ".pdf"
                                  ".txt" ".html" ".png" ".ini" ".cfg" 
                                  ".conf"))

;; load ido 
(require 'ido)
(setq ido-auto-merge-work-directories-length -1) ;; disable auto-merge
(setq ido-use-virtual-buffers t) ;; show recent files in buffer menu
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
(setq ido-decorations '("\n➔ " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n➔ " ""))

;; don't use ido for dired
(setq ido-read-file-name-non-ido '(dired))

;; color directories blue, firstmatch bold etc.
(set-face-attribute 'ido-first-match nil
                    :weight 'bold 
                    :height '1.125
                    :foreground "red")
(set-face-attribute 'ido-only-match nil
                    :weight 'bold 
                    :height '1.125
                    :foreground "ForestGreen")

(set-face-attribute 'ido-subdir nil
                    :foreground "blue")

;; set sensible keys for id in vertical mode
(setq ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))

;; use ido for kill-ring
(require 'kill-ring-ido)
(setq kill-ring-ido-shortage-length 20)

(global-set-key (kbd "M-y") 'kill-ring-ido)

;; show recently opened files
(add-hook 'after-init-hook
          '(lambda()
             (global-company-mode 1)
             (require 'recentf)
             (setq recentf-max-menu-items 50)
             (recentf-mode 1)))

(setq ido-use-virtual-buffers 'auto)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

  ;;; Completion hints for emacs functions
;; Horrible work-around to make smex work with emacs < 24.3:
;; remove this part when emacs is updated.
;; Check if Smex is supported
(when (equal (cons 1 1)
             (ignore-errors
               (subr-arity (symbol-function 'execute-extended-command))))
  (defun execute-extended-command (prefixarg &optional command-name)
    "Read function name, then read its arguments and call it."
    (interactive (list current-prefix-arg (read-extended-command)))
    (if (null command-name)
        (setq command-name (let ((current-prefix-arg prefixarg)) ; for prompt
                             (read-extended-command))))
    (let* ((function (and (stringp command-name) (intern-soft command-name)))
           (binding (and suggest-key-bindings
                         (not executing-kbd-macro)
                         (where-is-internal function overriding-local-map t))))
      (unless (commandp function)
        (error "`%s' is not a valid command name" command-name))
      (setq this-command function)
      (setq real-this-command function)
      (let ((prefix-arg prefixarg))
        (command-execute function 'record))
      (when binding
        (let* ((waited
                (sit-for (cond
                          ((zerop (length (current-message))) 0)
                          ((numberp suggest-key-bindings) suggest-key-bindings)
                          (t 2)))))
          (when (and waited (not (consp unread-command-events)))
            (with-temp-message
                (format "You can run the command `%s' with %s"
                        function (key-description binding))
              (sit-for (if (numberp suggest-key-bindings)
                           suggest-key-bindings
                         2)))))))))
;; end horrible hack

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

;;Use tab to C-TAB to complete
(eval-after-load "company"
  '(progn
     ;; don't start automatically 
     (setq company-idle-delay nil)
     ;; complete using C-TAB
     (global-set-key (kbd "<C-tab>") 'company-complete)
     ;; use C-n and C-p to cycle through completions
     ;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
     (define-key company-active-map (kbd "C-n") 'company-select-next)
     (define-key company-active-map (kbd "<tab>") 'company-select-next)
     (define-key company-active-map (kbd "C-p") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
     ;; enable math completions
     (require 'company-math)
     ;; company-mode completions for ess
     (require 'company-ess)
     (add-to-list 'company-backends 'company-math-symbols-unicode)
     (add-to-list 'company-backends 'company-math-symbols-latex)
     ;; ;; disable dabbrev
     ;; (delete 'company-dabbrev company-backends)
     ;; (delete 'company-dabbrev-code company-backends)
     ))

(add-hook 'after-init-hook 'global-company-mode)

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

(add-hook 'prog-mode-hook
          '(lambda()
             ;; make sure completion calls company-capf first
             (require 'company-capf)
             (set (make-local-variable 'company-backends)
                  (cons 'company-capf company-backends))
             (delete-dups company-backends)
             ))

;; require the main file containing common functions
(require 'eval-in-repl)
(setq comint-process-echoes t)

;; truncate lines in comint buffers
(add-hook 'comint-mode-hook
          '(lambda()
            (setq truncate-lines 1)))

;;;  ESS (Emacs Speaks Statistics)

;; Start R in the working directory by default
(setq ess-ask-for-ess-directory nil)

;; Scroll down when R generates output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; Make sure ESS is loaded
(require 'ess-site)

;; extra ESS stuff inspired by https://github.com/gaborcsardi/dot-emacs/blob/master/.emacs
(ess-toggle-underscore nil)
(defun my-ess-post-run-hook ()
  ;; reset output width when window is re-sized
  (add-hook 'inferior-ess-mode-hook
            '(lambda()
               (defun my-ess-execute-screen-options (foo)
                 (ess-execute-screen-options))
               (add-to-list
                'window-size-change-functions
                'my-ess-execute-screen-options)))
  )
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)

;; truncate long lines in R source files
(add-hook 'ess-mode-hook
          '(lambda()
             ;; don't wrap long lines
             (setq truncate-lines 1)
             ;; put company-capf at the front of the completion sources list
             (set (make-local-variable 'company-backends)
                  (cons 'company-capf company-backends))
             (delete-dups company-backends)
             ))

(add-hook 'R-mode-hook
          '(lambda()
             ;; make sure completion calls company-ess first
             (require 'company-ess)
             (set (make-local-variable 'company-backends)
                  (cons 'company-ess-backend company-backends))
             (delete-dups company-backends)
             ))

;; ;; try to get sane indentation
;; (setq ess-first-continued-statement-offset 2)
;; (setq ess-continued-statement-offset 0)
;; (setq ess-arg-function-offset-new-line 0)
;; (setq ess-arg-function-offset nil)
;; (setq ess-default-style 'DEFAULT)

;; Python completion and code checking
(setq elpy-modules '(elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-pyvenv
                     elpy-module-highlight-indentation
                     elpy-module-sane-defaults))
(elpy-enable)

;; use eval-in-repl to eval visibly in elpy buffers
(add-hook 'elpy-mode-hook
          '(lambda ()
             (require 'eval-in-repl-python)
             (define-key elpy-mode-map "\C-c\C-c" 'eir-eval-in-python)
             (setq company-idle-delay nil)))

;; ielm
(require 'eval-in-repl-ielm)
;; For .el files
(define-key emacs-lisp-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
;; For *scratch*
(define-key lisp-interaction-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
;; For M-x info
(define-key Info-mode-map "\C-c\C-c" 'eir-eval-in-ielm)

;; Set up completions
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             ;; make sure completion calls company-elisp first
             (require 'company-elisp)
             (set (make-local-variable 'company-backends)
                  (cons 'company-elisp company-backends))
             (delete-dups company-backends)
             ))

;;; markdown mode

;; Use markdown-mode for files with .markdown or .md extensions
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
   (latex . t)
   (octave . t)
   (ditaa . t)
   (org . t)
   (perl . t)
   (julia . t)
))

;; Set sensible mode for editing dot files
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Fontify code blocks in org-mode
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)

(require 'org-capture)
(require 'org-protocol)
(require 'ob-stata)

;; set up capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/notes.org" "RT Tasks")
         "* TODO %?\n  %i\n  %a")))

(define-key global-map "\C-cc" 'org-capture)

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

;;; Dired and Dired+ configuration

;; load dired+ and mouse3
(require 'dired+)
(require 'mouse3)

;; set dired listing options
(setq dired-listing-switches "-alDhp")

;; more subdued colors
(set-face-attribute 'diredp-ignored-file-name nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-read-priv nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-write-priv nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-other-priv nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-rare-priv nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-no-priv nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-exec-priv nil
                    :foreground "LightGray"
                    :background nil)
(set-face-attribute 'diredp-file-name nil
                    :weight 'bold
                    :background nil)
(set-face-attribute 'diredp-dir-priv nil
                    :weight 'bold)
(set-face-attribute 'diredp-file-suffix nil
                    :foreground nil)
                    
;; make sure dired buffers end in a slash so we can identify them easily
(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook
          '(lambda()
             (setq truncate-lines 1)))

;; open files in external programs
;; (from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let (doIt
        (myFileList
         (cond
          ((string-equal major-mode "dired-mode")
           (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? "))) 
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)))
         myFileList))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath)
           (shell-command (format "open \"%s\"" fPath)))
         myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" fPath))) myFileList))))))
;; open files from dired with "E"
(define-key dired-mode-map (kbd "E") 'xah-open-in-external-app)
;; use zip/unzip to compress/uncompress zip archives
(eval-after-load "dired-aux"
 '(add-to-list 'dired-compress-file-suffixes 
               '("\\.zip\\'" "" "unzip")))

;; shell
(require 'essh) ; if not done elsewhere; essh is in the local lisp folder
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          '(lambda()
             (local-set-key "\C-c\C-c" 'eir-eval-in-shell)))


;; Automatically adjust output width in commint buffers
;; from http://stackoverflow.com/questions/7987494/emacs-shell-mode-display-is-too-wide-after-splitting-window
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

(defun my-shell-mode-hook ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))
  ;; auto-complete for shell-mode (linux only)
(if (eq system-type 'gnu/linux)
    (progn 
      (setq explicit-shell-file-name "bash")
      (setq explicit-bash-args '("-c" "-t" "export EMACS=; stty echo; bash"))  
      (ansi-color-for-comint-mode-on)
      (add-hook 'shell-mode-hook
          '(lambda()
             ;; make sure completion calls company-readline first
             (require 'readline-complete)
             (set (make-local-variable 'company-backends)
                  (cons 'company-readline company-backends))
             (delete-dups company-backends)
             ))
      (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))))

(add-hook 'shell-mode-hook
          '(lambda()
             ;; add this hook as buffer local, so it runs once per window.
             (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)))

;; extra completion for eshell
(add-hook 'eshell-mode-hook
          '(lambda()
             (require 'pcmpl-args)
             (require 'pcmpl-pip)
             ;; programs that don't work well in eshell and should be run in visual mode
             (add-to-list 'eshell-visual-commands "ssh")
             (add-to-list 'eshell-visual-commands "tail")
             (add-to-list 'eshell-visual-commands "htop")
             (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))))

;;; Misc. Conveniences

;; window arrangement history
(setq winner-dont-bind-my-keys t) 
(winner-mode 1)

;;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)                      
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; start the server if not already started
(add-hook 'after-init-hook
                  '(lambda ()
                     (load "server")
                     (unless (server-running-p) (server-start))))

;; ;; use regex search by default
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)

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

;; enable toggling paragraph un-fill
;; from http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(define-key global-map "\M-Q" 'unfill-paragraph)

;; line wrapping
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'text-mode-hook 'visual-line-mode 1)
(add-hook 'prog-mode-hook
          '(lambda()
              (setq truncate-lines 1)))

;; don't require two spaces for sentence end.
(setq sentence-end-double-space nil)

;; Use CUA mode only for handy rectangle features
(cua-selection-mode t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; The beeping can be annoying--turn it off
(set-variable 'visible-bell t)

;; save settings made using the customize interface to a sparate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region ";; Put user configuration here" nil custom-file))
(load custom-file 'noerror)

;; byte-compile init file if needed
(add-hook 'after-init-hook
          (lambda ()
            (byte-recompile-file user-init-file nil 1 nil)
            (switch-to-buffer "*scratch*")))

;; cleanup
(switch-to-buffer "*scratch*")
(delete-other-windows)
(redisplay t)
