(when (< (string-to-number 
           (concat 
            (number-to-string emacs-major-version) 
            "." 
            (number-to-string emacs-minor-version)))
          24.5)
  (error "Your version of emacs is very old and must be upgraded before you can use these packages!"))

;; set coding system so emacs doesn't choke on melpa file listings
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(require 'cl)

;; set things that need to be set before packages load
(setq outline-minor-mode-prefix "\C-c\C-o")
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key "\C-c\C-o"
                                    outline-mode-prefix-map)))

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
                        windresize
                        diff-hl
                        adaptive-wrap
                        ;; melpa packages
                        undo-tree
                        better-defaults
                        diminish
                        smart-mode-line
                        dired+
                        ace-window
                        howdoi
                        auctex-latexmk
                        multi-term
                        with-editor
                        eyebrowse
                        mouse3
                        swiper
                        counsel
                        flx-ido
                        smex
                        ivy-bibtex
                        which-key
                        outline-magic
                        smooth-scroll
                        unfill
                        company
                        ess
                        markdown-mode
                        polymode
                        eval-in-repl
                        haskell-mode
                        ghc
                        company-ghci
                        flycheck
                        scala-mode
                        ensime
                        sbt-mode
                        exec-path-from-shell
                        htmlize
                        ;; org-mode packages
                        org-plus-contrib))

;; Activate package autoloads
(package-initialize)
(setq package-initialize nil)

;; make sure stale packages don't get loaded
(dolist (package my-package-list)
  (if (featurep package)
      (unload-feature package t)))
;; Install packages in package-list if they are not already installed
(unless (every #'package-installed-p my-package-list)
  (package-refresh-contents)
  (dolist (package my-package-list)
    (when (not (package-installed-p package))
      (package-install package))))

;; mode line theme
(add-hook 'after-init-hook 'sml/setup)
(setq sml/theme 'light)

;; add custom lisp directory to path
(let ((default-directory (concat user-emacs-directory "lisp/")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; on OSX Emacs needs help setting up the system paths
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; better defaults are well, better... but we don't always agree
(menu-bar-mode 1)

;; Mouse scrolling behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode 1) ; makes the region visible
(line-number-mode 1)    ; makes the line number show up
(column-number-mode 1)  ; makes the column number show up

;; ;; smooth scrolling with C-up/C-down
(require 'smooth-scroll)
(smooth-scroll-mode)
(global-set-key [(control down)] 'scroll-up-1)
(global-set-key [(control up)] 'scroll-down-1)
(global-set-key [(control left)] 'scroll-right-1)
(global-set-key [(control right)] 'scroll-left-1)

;; enable toggling paragraph un-fill
(define-key global-map "\M-Q" 'unfill-paragraph)

;; line wrapping
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'text-mode-hook 'visual-line-mode 1)
(add-hook 'prog-mode-hook
          (lambda()
            (toggle-truncate-lines t)
              (outline-minor-mode t)))

;; don't require two spaces for sentence end.
(setq sentence-end-double-space nil)

;; The beeping can be annoying--turn it off
(set-variable 'visible-bell t)

;; Use CUA mode to make life easier. We _do_ use standard copy/paste etc. 
  (cua-mode t)

  ;; (cua-selection-mode t) ;; uncomment this to get cua goodness without copy/paste etc.

  ;; ;; Make control-z undo
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)

  ;; ;; 
  ;; Make right-click do something close to what people expect
  (global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)
  ;; (global-set-key (kbd "C-f") 'isearch-forward)
  ;; (global-set-key (kbd "C-s") 'save-buffer)
  ;; (global-set-key (kbd "C-o") 'counsel-find-file)
  (define-key cua-global-keymap (kbd "<C-S-SPC>") nil)
  (define-key cua-global-keymap (kbd "<C-return>") nil)
  (setq cua-rectangle-mark-key (kbd "<C-S-SPC>"))
  (define-key cua-global-keymap (kbd "<C-S-SPC>") 'cua-rectangle-mark-mode)

;; Work spaces
(setq eyebrowse-keymap-prefix (kbd "C-c C-l"))
(eyebrowse-mode t)

;; Undo/redo window changes
(winner-mode 1)

;; use ace-window for navigating windows
(global-set-key (kbd "C-x O") 'ace-window)
(with-eval-after-load "ace-window"
  (set-face-attribute 'aw-leading-char-face nil :height 2.5))

;; enable on-the-fly spell checking
(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)))
;; prevent flyspell from finding mistakes in the code
(add-hook 'prog-mode-hook
          (lambda ()
            ;; `ispell-comments-and-strings'
            (flyspell-prog-mode)))

;; ispell should not check code blocks in org mode
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))

(when (eq system-type 'gnu/linux)
  (setq hfyview-quick-print-in-files-menu t)
  (require 'hfyview)
  (setq mygtklp (executable-find "gtklp"))
  (when mygtklp
    (setq lpr-command "gtklp")
    (setq ps-lpr-command "gtklp")))

(when (eq system-type 'darwin)
  (setq hfyview-quick-print-in-files-menu t)
  (require 'hfyview))

(ivy-mode 1)

(setq counsel-find-file-ignore-regexp "\\`\\.")
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style nil)

;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-S-v") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-o") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "<C-tab>") 'counsel-company)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; Ivy-resume and other commands

(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; Make Ivy more like ido
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-d") 'ivy-done)
(define-key ivy-minibuffer-map (kbd "C-b") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "C-f") 'ivy-immediate-done)

;; show recently opened files
(setq recentf-max-menu-items 50)
(recentf-mode 1)

(require 'company)
;; cancel if input doesn't match, be patient, and don't complete automatically.
(setq company-require-match nil
      company-async-timeout 6
      company-idle-delay nil)
;; complete using C-tab
(global-set-key (kbd "<C-tab>") 'counsel-company)
;; use C-n and C-p to cycle through completions
;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

(require 'company-capf)
;; put company-capf and company-files at the beginning of the list
(setq company-backends
      (delete-dups (cons 'company-files (cons 'company-capf company-backends))))
(setq-default company-backends
              (delete-dups (cons 'company-files (cons 'company-capf company-backends))))

;;Use tab to complete.
;; From https://github.com/company-mode/company-mode/issues/94
(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-for-tab-command)

(setq tab-always-indent 'complete)

(defvar completion-at-point-functions-saved nil)

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete)))

;; not sure why this should be set in a hook, but that is how the manual says to do it.
(add-hook 'after-init-hook 'global-company-mode)

;; (require 'which-key)
(which-key-mode)

;; (require 'flycheck)
(global-flycheck-mode)

;;; Configure outline minor modes
;; Less crazy key bindings for outline-minor-mode
(setq outline-minor-mode-prefix "\C-c\C-o")
;; load outline-magic along with outline-minor-mode
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
            (define-key outline-minor-mode-map "\C-c\C-o\t" 'outline-cycle)))

;; require the main file containing common functions
(require 'eval-in-repl)
(setq comint-process-echoes t)

;; truncate lines in comint buffers
(add-hook 'comint-mode-hook
          (lambda()
            (setq truncate-lines 1)))

;; Scroll down for input and output
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;;;  ESS (Emacs Speaks Statistics)

;; Start R in the working directory by default
(setq ess-ask-for-ess-directory nil)

;; Make sure ESS is loaded before we configure it
(autoload 'julia "ess-julia" "Start a Julia REPL." t)
(with-eval-after-load "ess-site"
  ;; disable ehoing input
  (setq ess-eval-visibly nil)
  ;; Start R in the working directory by default
  (setq ess-ask-for-ess-directory nil)
  ;; Use tab completion
  (setq ess-tab-complete-in-script t)
  ;; extra ESS stuff inspired by https://github.com/gaborcsardi/dot-emacs/blob/master/.emacs
  (ess-toggle-underscore nil)
  (defun my-ess-execute-screen-options (foo)
    "cycle through windows whose major mode is inferior-ess-mode and fix width"
    (interactive)
    (setq my-windows-list (window-list))
    (while my-windows-list
      (when (with-selected-window (car my-windows-list) (string= "inferior-ess-mode" major-mode))
        (with-selected-window (car my-windows-list) (ess-execute-screen-options t)))
      (setq my-windows-list (cdr my-windows-list))))
  (add-to-list 'window-size-change-functions 'my-ess-execute-screen-options)
  (define-key ess-mode-map (kbd "<C-return>") 'ess-eval-region-or-function-or-paragraph-and-step)
  ;; truncate long lines in R source files
  (add-hook 'ess-mode-hook
            (lambda()
              ;; don't wrap long lines
              (toggle-truncate-lines t)
              (outline-minor-mode t))))

(with-eval-after-load "python"
  ;; simple evaluation with C-ret
  (require 'eval-in-repl-python)
  (define-key python-mode-map "\C-c\C-c" 'eir-eval-in-python)
  (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python))

(with-eval-after-load "elisp-mode"
  (require 'company-elisp)
  ;; ielm
  (require 'eval-in-repl-ielm)
  ;; For .el files
  (define-key emacs-lisp-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; For *scratch*
  (define-key lisp-interaction-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  ;; For M-x info
  (define-key Info-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
  ;; Set up completions
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              ;; make sure completion calls company-elisp first
              (require 'company-elisp)
              (setq-local company-backends
                          (delete-dups (cons 'company-elisp (cons 'company-files company-backends)))))))

(require 'company-ghci)
(add-hook 'haskell-mode-hook (lambda ()
                               (setq-local company-backends
                                           (delete-dups (cons 'company-ghci (cons 'company-files company-backends))))))
(add-hook 'haskell-interactive-mode-hook 'company-mode)

;; Use markdown-mode for files with .markdown or .md extensions
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; AucTeX config
(with-eval-after-load "Latex"
  ;; Easy compile key
  (define-key LaTeX-mode-map (kbd "<C-return") 'TeX-command-run-all)
  ;; Allow paragraph filling in tables
  (setq LaTeX-indent-environment-list
        (delq (assoc "table" LaTeX-indent-environment-list)
              LaTeX-indent-environment-list))
  (setq LaTeX-indent-environment-list
        (delq (assoc "table*" LaTeX-indent-environment-list)
              LaTeX-indent-environment-list))
  ;; Misc. latex settings
  (setq TeX-parse-self t
        TeX-auto-save t)
  (setq-default TeX-master nil)
  ;; Add beamer frames to outline list
  (setq TeX-outline-extra
        '((".*\\\\begin{frame}\n\\|.*\\\\begin{frame}\\[.*\\]\\|.*\\\\begin{frame}.*{.*}\\|.*[       ]*\\\\frametitle\\b" 3)))
  ;; reftex settings
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-reftex)
              (TeX-PDF-mode t)
              (LaTeX-math-mode)
              (TeX-source-correlate-mode t)
              (imenu-add-to-menubar "Index")
              (outline-minor-mode)))
  (add-hook 'bibtex-mode-hook
            (lambda ()
              (define-key bibtex-mode-map "\M-q" 'bibtex-fill-entry))))

(setq ivy-bibtex-default-action 'bibtex-completion-insert-citation)
(global-set-key (kbd "C-c r") 'ivy-bibtex)

(with-eval-after-load "org"
  (setq org-replace-disputed-keys t)
  (setq org-support-shift-select t)
  (setq org-export-babel-evaluate nil)
  ;; (setq org-startup-indented t)
  ;; increase imenu depth to include third level headings
  (setq org-imenu-depth 3)
  ;; Set sensible mode for editing dot files
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  ;; Update images from babel code blocks automatically
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  ;; configure org-mode when opening first org-mode file
  ;; Load additional export formats
  (require 'ox-odt)
  (require 'ox-md)
  (require 'ox-freemind)
  (require 'ox-bibtex)
  ;; Enable common programming language support in org-mode
  (require 'ess-site)
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
  ;; Fontify code blocks in org-mode
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (require 'org-capture)
  (require 'org-protocol)
  (require 'ob-stata))

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

(when (executable-find "mu")
  (autoload 'mu4e "mu4e" "Read your mail." t)
  (with-eval-after-load "mu4e"
    (setq mu4e-headers-include-related t
          mu4e-headers-skip-duplicates t
          mu4e-headers-fields '(
                                (:human-date . 12)
                                (:flags . 6)
                                (:mailing-list . 10)
                                (:from-or-to . 22)
                                (:thread-subject)))
    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)
    ;; enable notifications
    (setq mu4e-enable-mode-line t)
    ;; use org for composing rich text emails
    (require 'org-mu4e)
    (setq org-mu4e-convert-to-html t)
    (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
    (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)
    ;; render html
    (require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text)))

;;; Dired and Dired+ configuration
(add-hook 'dired-mode-hook 
          (lambda()
            (diff-hl-dired-mode)
            (diff-hl-margin-mode)))

;; show details by default
(setq diredp-hide-details-initially-flag nil)

;; set dired listing options
(if (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-alDhp"))

;; make sure dired buffers end in a slash so we can identify them easily
(defun ensure-buffer-name-ends-in-slash ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat name "/") t))))
(add-hook 'dired-mode-hook 'ensure-buffer-name-ends-in-slash)
(add-hook 'dired-mode-hook
          (lambda()
             (setq truncate-lines 1)))

;; open files in external programs
;; (from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
;; consider replacing with https://github.com/thamer/runner
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
;; use zip/unzip to compress/uncompress zip archives
(with-eval-after-load "dired-aux"
  (add-to-list 'dired-compress-file-suffixes 
               '("\\.zip\\'" "" "unzip"))
  ;; open files from dired with "E"
  (define-key dired-mode-map (kbd "E") 'xah-open-in-external-app))

;; term
(with-eval-after-load "term"
(define-key term-mode-map (kbd "C-j") 'term-char-mode)
(define-key term-raw-map (kbd "C-j") 'term-line-mode))

(with-eval-after-load "multi-term"
(define-key term-mode-map (kbd "C-j") 'term-char-mode)
(define-key term-raw-map (kbd "C-j") 'term-line-mode))

;; shell
(require 'essh) ; if not done elsewhere; essh is in the local lisp folder
(require 'eval-in-repl-shell)
(add-hook 'sh-mode-hook
          (lambda()
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

(add-hook 'shell-mode-hook
          (lambda()
             ;; add this hook as buffer local, so it runs once per window.
             (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)))

;; extra completion for eshell
(add-hook 'eshell-mode-hook
          (lambda()
             ;; programs that don't work well in eshell and should be run in visual mode
             (add-to-list 'eshell-visual-commands "ssh")
             (add-to-list 'eshell-visual-commands "tail")
             (add-to-list 'eshell-visual-commands "htop")
             (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))))

;; Use emacs as editor when running external processes or using shells in emacs
(require 'with-editor)
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

(shell-command-with-editor-mode t)

;; always use fancy-startup, even on small screens
  ;; but only if running in windowed mode
  (defun always-use-fancy-splash-screens-p () 1)
  (defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
  (add-hook 'after-init-hook
            (lambda()
              (if inhibit-startup-screen
                  (add-hook 'emacs-startup-hook 
                            (lambda() (switch-to-buffer "*scratch*"))))))

(add-to-list 'fancy-startup-text
             '(:face
               (variable-pitch default)
               "\nYou are running a customized Emacs configuration. See "  :link
               ("here"
                #[257 "\300\301!\207"
                      [browse-url-default-browser "http://github.com/izahn/dotemacs/"]
                      3 "\n\n(fn BUTTON)"]
                "Open the README file")
               "\nfor information about these customizations.\n"))

;; save settings made using the customize interface to a sparate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region ";; Put user configuration here" nil custom-file))
(load custom-file 'noerror)

;; ;; clean up the mode line
; (require 'diminish)
;; (diminish 'company-mode)
(diminish 'google-this-mode)
(diminish 'outline-minor-mode)
(diminish 'smooth-scroll-mode)
