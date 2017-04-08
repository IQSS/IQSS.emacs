(when (< (string-to-number 
           (concat 
            (number-to-string emacs-major-version) 
            "." 
            (number-to-string emacs-minor-version)))
          25.1)
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
(setq save-abbrevs 'silently)

;; load the package manager
(require 'package)
(package-initialize t)

;; Add additional package sources
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Make a list of the packages you want
(setq package-selected-packages
      '(;; gnu packages
        auctex
        windresize
        diff-hl
        adaptive-wrap
        ;; melpa packages
        ;; mode-icons ; slows things down, can be buggy
        command-log-mode
        undo-tree
        better-defaults
        diminish
        dired+
        ace-window
        howdoi
        auctex-latexmk
        multi-term
        with-editor
        git-commit
        magit
        eyebrowse
        mouse3
        swiper
        counsel
        flx-ido
        smex
        ivy-bibtex
        hydra
        ivy-hydra
        which-key
        outline-magic
        smooth-scroll
        unfill
        company
        company-math
        company-auctex
        ess
        markdown-mode
        polymode
        eval-in-repl
        haskell-mode
        ghc
        company-ghci
        dante
        flycheck
        scala-mode
        ensime
        sbt-mode
        exec-path-from-shell
        htmlize
        sdcv ;; stardictionary
        osx-dictionary
        define-word
        ox-pandoc
        untitled-new-buffer))
;; install packages if needed
(unless (every 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  ;; org needs to be installed first
  (package-install (cadr (assq 'org package-archive-contents)))
  (package-install-selected-packages))
(package-initialize)

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
(scroll-bar-mode 1)

;; scrolling behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 100000)

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode 1) ; makes the region visible
(line-number-mode 1)    ; makes the line number show up
(column-number-mode 1)  ; makes the column number show up

;; smooth horizontal scrolling
(require 'smooth-scroll)
(global-set-key [(control down)] 'scroll-up-1)
(global-set-key [(control up)] 'scroll-down-1)
(global-set-key [(control left)] 'scroll-right-1)
(global-set-key [(control right)] 'scroll-left-1)

;; make home and end behave
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; enable toggling paragraph un-fill
(define-key global-map "\M-Q" 'unfill-paragraph)

;;; line wrapping
;; neck beards be damned, we don't need to hard wrap. The editor can soft wrap for us.
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'text-mode-hook 'visual-line-mode 1)
(add-hook 'prog-mode-hook
          (lambda()
            (toggle-truncate-lines t)
              (outline-minor-mode t)))

;; indicate visual-line-mode wrap
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; but be gentle
(defface visual-line-wrap-face
'((t (:foreground "gray")))
"Face for visual line indicators.")
(set-fringe-bitmap-face 'left-curly-arrow 'visual-line-wrap-face)
(set-fringe-bitmap-face 'right-curly-arrow 'visual-line-wrap-face)

;; don't require two spaces for sentence end.
(setq sentence-end-double-space nil)

;; The beeping can be annoying--turn it off
(setq visible-bell t)

;; save place -- move to the place I was last time I visited this file
(save-place-mode t)

;; easy navigation in read-only buffers
(setq view-read-only t)
(with-eval-after-load "view-mode"
  (define-key view-mode-map (kbd "s") 'swiper))

;; Use CUA mode to make life easier. We _do_ use standard copy/paste etc. 
(cua-mode t)

;; (cua-selection-mode t) ;; uncomment this to get cua goodness without copy/paste etc.

;; make control-q quit
(global-set-key (kbd "C-S-q") 'save-buffers-kill-terminal)

;; make control-w close window
(global-set-key (kbd "C-w") 'delete-window)
(define-key cua--region-keymap (kbd "C-w") 'cua-cut-region)

;; new buffer (not quite what you expect, but we emacs users need C-n
(setq untitled-new-buffer-major-modes '(text-mode r-mode python-mode LaTeX-mode markdown-mode org-mode))
(global-set-key (kbd "C-S-n") 'untitled-new-buffer-with-select-major-mode)

;; ;; Make control-z undo
(global-undo-tree-mode t)
(global-set-key (kbd "C-z") 'undo)
(define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-x u") 'undo)
(define-key undo-tree-map (kbd "C-x U") 'undo-tree-visualize)
(define-key undo-tree-map (kbd "M-z") 'undo-tree-visualize)
;; Make C-g quit undo tree
(define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map (kbd "<escape> <escape> <escape>") 'undo-tree-visualizer-quit)

;;
;; Make right-click do something close to what people expect
(global-set-key (kbd "<mouse-3>") 'mouse3-popup-menu)
;; (global-set-key (kbd "C-f") 'isearch-forward)
;; (global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-o") 'counsel-find-file)
(define-key cua-global-keymap (kbd "<C-S-SPC>") nil)
(define-key cua-global-keymap (kbd "<C-return>") nil)
(setq cua-rectangle-mark-key (kbd "<C-S-SPC>"))
(define-key cua-global-keymap (kbd "<C-S-SPC>") 'cua-rectangle-mark-mode)

;; nicer mode line
;; (mode-icons-mode)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; page up/down
(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>") 'end-of-buffer)

;; Work spaces
(setq eyebrowse-keymap-prefix (kbd "C-c C-l"))
(eyebrowse-mode t)

;; Undo/redo window changes
(winner-mode 1)

;; windmove 
(global-set-key (kbd "C-x <S-left>") 'windmove-left)
(global-set-key (kbd "C-x <S-right>") 'windmove-right)
(global-set-key (kbd "C-x <S-up>") 'windmove-up)
(global-set-key (kbd "C-x <S-down>") 'windmove-down)

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

;; Dictionaries

;; default in case we don't find something local
(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c S-D") 'define-word)

;; use dictionary app on os x
(when (memq window-system '(mac ns))
  (global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point)
  (global-set-key (kbd "C-c S-D") 'osx-dictionary-search-input))

;; Use stardict if we find a usable interface
(when (executable-find "sdcv")
  (require 'sdcv)
  (global-set-key (kbd "C-c d") 'sdcv-search-input)
  (global-set-key (kbd "C-c S-D") 'sdcv-search-pointer+)
  (add-hook 'sdcv-mode-hook
            '(lambda()
               (setq-local font-lock-string-face 'default))))

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

;; make sure we wrap in the minibuffer
(add-hook 'minibuffer-setup-hook '(lambda() (setq truncate-lines nil)))

(ivy-mode 1)

(setq counsel-find-file-ignore-regexp "\\`\\.")
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; (setq ivy-display-style nil)

;; Ivy-based interface to standard commands
(global-set-key (kbd "C-h b") 'counsel-descbinds)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
;; Search files in directory with C-S
(global-set-key (kbd "C-S-s") 'find-grep-dired); default if we don't find something better
(cond
 ((executable-find "rg") ; search with ripgrep if we have it
  (global-set-key (kbd "C-S-s") 'counsel-rg))
 ((executable-find "ag") ; otherwise search with ag if we have it
  (global-set-key (kbd "C-S-s") 'counsel-ag))
 ((executable-find "pt") ; otherwise search with pt if we have it
  (global-set-key (kbd "C-S-s") 'counsel-pt)))
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-S-v") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-o") 'counsel-find-file)
;; search for files to open with "C-O=
(when (memq window-system '(mac ns)) ; use mdfind on Mac. TODO: what about windows?
  (setq locate-command "mdfind")
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))
(global-set-key (kbd "C-x C-S-F") 'find-name-dired) ; default in case we don't have something better
(global-set-key (kbd "C-x C-S-F") 'counsel-locate)
(global-set-key (kbd "C-S-O") 'counsel-locate)
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

(global-set-key (kbd "C-c i") 'ivy-resume)

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
      company-idle-delay nil
      company-global-modes '(not term-mode))
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
      '(company-files company-capf company-nxml company-css company-cmake company-semantic company-clang company-xcode company-eclim))
(setq-default company-backends
              '(company-files company-capf company-nxml company-css company-cmake company-semantic company-clang company-xcode company-eclim))

;;Use tab to complete.
;; See https://github.com/company-mode/company-mode/issues/94 for another approach.

;; this is a copy-paste from the company-package with extra conditions to make
;; sure we don't offer completions in the middle of a word.

(defun my-company-indent-or-complete-common ()
  "Indent the current line or region, or complete the common part."
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((and (not (looking-at "\\w\\|\\s_"))
         (memq indent-line-function
               '(indent-relative indent-relative-maybe)))
    (company-complete-common))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (call-interactively #'indent-for-tab-command)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 (not (looking-at "\\w\\|\\s_")))
        (company-complete-common))))))

(define-key company-mode-map (kbd "<tab>") 'my-company-indent-or-complete-common)

;; not sure why this should be set in a hook, but that is how the manual says to do it.
(add-hook 'after-init-hook 'global-company-mode)

;; (require 'which-key)
(which-key-mode)

;; (require 'flycheck)
;; (global-flycheck-mode)

;;; Configure outline minor modes
;; Less crazy key bindings for outline-minor-mode
(setq outline-minor-mode-prefix "\C-c\C-o")
;; load outline-magic along with outline-minor-mode
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
            (define-key outline-minor-mode-map "\C-c\C-o\t" 'outline-cycle)))

(setq command-log-mode-auto-show t)
(global-set-key (kbd "C-x cl") 'global-command-log-mode)

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
  ;; see https://github.com/emacs-ess/ESS/pull/390 for ideas on how to integrate tab completion
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
  (define-key ess-mode-map (kbd "<C-S-return>") 'ess-eval-buffer)
  ;; truncate long lines in R source files
  (add-hook 'ess-mode-hook
            (lambda()
              ;; don't wrap long lines
              (toggle-truncate-lines t)
              (outline-minor-mode t)))
  ;; highlight function calls and operators
  (setq ess-R-font-lock-keywords
        (quote
         ((ess-R-fl-keyword:modifiers)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . 1)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T)
          (ess-R-fl-keyword:%op% . t)))))

(defalias 'python 'run-python)
(with-eval-after-load "python"
  ;; try to get indent/completion working nicely
  (setq python-indent-trigger-commands '(my-company-indent-or-complete-common indent-for-tab-command yas-expand yas/expand))
  ;; readline support is wonky at the moment
  (setq python-shell-completion-native-enable nil)
  ;; simple evaluation with C-ret
  (require 'eval-in-repl-python)
  (define-key python-mode-map (kbd "C-c C-c") 'eir-eval-in-python)
  (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)
  (define-key python-mode-map (kbd "C-c C-b") 'python-shell-send-buffer)
  (define-key python-mode-map (kbd "<C-S-return>") 'python-shell-send-buffer))

(with-eval-after-load "elisp-mode"
  (require 'company-elisp)
  ;; ielm
  (require 'eval-in-repl-ielm)
  ;; For .el files
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
  (define-key emacs-lisp-mode-map (kbd "<C-S-return>") 'eval-buffer)
  ;; For *scratch*
  (define-key lisp-interaction-mode-map "\C-c\C-c" 'eir-eval-in-ielm)
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eir-eval-in-ielm)
  (define-key lisp-interaction-mode-map (kbd "C-c C-b") 'eval-buffer)
  (define-key lisp-interaction-mode-map (kbd "<C-S-return>") 'eval-buffer)
  ;; For M-x info
  (define-key Info-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
  ;; Set up completions
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              ;; make sure completion calls company-elisp first
              (require 'company-elisp)
              (setq-local company-backends
                          (delete-dups (cons 'company-elisp (cons 'company-files company-backends)))))))

(defalias 'haskell 'haskell-interactive-bring)

(add-hook 'haskell-mode-hook (lambda ()
                               (dante-mode)
                               (setq-local company-backends
                                           (delete-dups (cons 'company-ghci (cons 'company-files company-backends))))))
(add-hook 'haskell-interactive-mode-hook 'company-mode)

;; Use markdown-mode for files with .markdown or .md extensions
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; AucTeX config
(with-eval-after-load "Latex"
  ;; Easy compile key
  (define-key LaTeX-mode-map (kbd "<C-return>") 'TeX-command-run-all)
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
  (add-hook 'TeX-mode-hook
            (lambda ()
              (turn-on-reftex)
              (TeX-PDF-mode t)
              (LaTeX-math-mode)
              (TeX-source-correlate-mode t)
              (imenu-add-to-menubar "Index")
              (outline-minor-mode)
              (require 'company-math)
              (require 'company-auctex)
              (company-auctex-init)
              (setq-local company-backends (delete-dups
                                            (cons '(company-math-symbols-latex
                                                    company-auctex-macros
                                                    company-auctex-environments)
                                                  (cons 'company-files company-backends))))))    
  (add-hook 'bibtex-mode-hook
            (lambda ()
              (define-key bibtex-mode-map "\M-q" 'bibtex-fill-entry))))

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
(global-set-key (kbd "C-c r") 'ivy-bibtex)

(with-eval-after-load "org"
  ;; no compay mode in org buffers
  (add-hook 'org-mode-hook (lambda() (company-mode -1)))
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
  (require 'ox-ascii)
  (require 'ox-md)
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-odt)

  (require 'org-capture)
  (require 'org-protocol)

  ;; Enable common programming language support in org-mode
  (require 'ob-shell)
  (require 'ob-emacs-lisp)
  (require 'ob-org)
  (when (executable-find "R") 
      (require 'ess-site)
      (require 'ob-R))
  (when (executable-find "python") (require 'ob-python))
  (when (executable-find "matlab") (require 'ob-matlab))
  (when (executable-find "octave") (require 'ob-octave))
  (when (executable-find "perl") (require 'ob-perl))
  (when (executable-find "dot") (require 'ob-dot))
  (when (executable-find "ditaa") (require 'ob-ditaa))

  ;; Fontify code blocks in org-mode
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil))

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
  (add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))
  ;; polymode doesn't play nice with adaptive-wrap, turn it off
  (add-hook 'polymode-init-host-hook
            '(lambda()
               (adaptive-wrap-prefix-mode -1)
               (electric-indent-mode -1)
               (unless (featurep 'ess-site)
                 (require 'ess-site)))))

(when (executable-find "mu")
  (autoload 'mu4e "mu4e" "Read your mail." t)
  (with-eval-after-load "mu4e"
    (require 'mu4e)
    (require 'mu4e-headers)
    (setq mu4e-headers-include-related t
          mu4e-headers-skip-duplicates t
          ;; don't keep message buffers around
          message-kill-buffer-on-exit t
          ;; enable notifications
          mu4e-enable-mode-line t
          mu4e-headers-fields '(
                                (:human-date . 12)
                                (:flags . 6)
                                ;; (:mailing-list . 10)
                                (:from-or-to . 22)
                                (:subject)))
    ;; ;; use org for composing rich text emails
    ;; (require 'org-mu4e)
    ;; (setq org-mu4e-convert-to-html t)
    ;; (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
    ;; (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)
    ;; ;; re
    nder html
    (require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text)
    (add-hook 'mu4e-view-mode-hook 'visual-line-mode)))

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

;; multi-term
(defalias 'terminal 'multi-term)
(with-eval-after-load "multi-term"
  (define-key term-mode-map (kbd "C-j") 'term-char-mode)
  (define-key term-raw-map (kbd "C-j") 'term-line-mode))

;; shell
(require 'essh) ; if not done elsewhere; essh is in the local lisp folder
(require 'eval-in-repl-shell)
(with-eval-after-load "sh-script"
  (define-key sh-mode-map "\C-c\C-c" 'eir-eval-in-shell)
  (define-key sh-mode-map (kbd "<C-return>") 'eir-eval-in-shell)
  (define-key sh-mode-map (kbd "<C-S-return>") 'executable-interpret))

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
(when (and (string-match-p "remacs" (prin1-to-string (frame-list)))
           (executable-find "remacsclient"))
  (setq with-editor-emacsclient-executable (executable-find "remacsclient")))

(require 'with-editor)
(add-hook 'shell-mode-hook
          (lambda()
            (with-editor-export-editor)
            (with-editor-export-git-editor)
            (sleep-for 0.5) ; this is bad, but thinking hurts and it works.
            (call-interactively 'comint-clear-buffer)))
(add-hook 'term-exec-hook
          (lambda()
            (with-editor-export-editor)
            (with-editor-export-git-editor)
            (sleep-for 0.5) ; see comment above
            (call-interactively 'comint-clear-buffer)))
(add-hook 'eshell-mode-hook
          (lambda()
            (with-editor-export-editor)
            (with-editor-export-git-editor)))

(shell-command-with-editor-mode t)
(require 'git-commit)

;; save settings made using the customize interface to a sparate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region ";; Put user configuration here" nil custom-file))
(load custom-file 'noerror)

;; ;; clean up the mode line
; (require 'diminish)
(diminish 'visual-line-mode)
(diminish 'which-key-mode)
(diminish 'company-mode "comp")
(diminish 'outline-minor-mode "outln")
(diminish 'undo-tree-mode)


;; No, we do not need the splash screen
(setq inhibit-startup-screen t)

;; start with untitled new buffer
(add-hook 'after-init-hook
          '(lambda()
                 (untitled-new-buffer-with-select-major-mode 'text-mode)))

(setq untitled-new-buffer-major-modes '(text-mode emacs-lisp-mode))
;; Change default buffer name.
(setq untitled-new-buffer-default-name "Untitled")
