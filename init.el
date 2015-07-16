
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

;; use desktop mode, but only for frame layout
;; and only if running in windowed mode
(when (display-graphic-p)
  (setq desktop-load-locked-desktop t)
  (setq desktop-buffers-not-to-save "^.*$")
  (setq desktop-files-not-to-save "^.*$")
  (setq desktop-save t)
  (setq desktop-auto-save-timeout nil)
  (setq desktop-globals-to-save nil)
  (setq desktop-locals-to-save nil)
  (desktop-save-mode 1)
  ;; always use fancy-startup, even on small screens
  ;; but only if running in windowed mode
  (defun always-use-fancy-splash-screens-p () 1)
  (defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
  (add-hook 'after-init-hook
            (lambda()
              (if inhibit-startup-screen
                  (add-hook 'emacs-startup-hook 
                            (lambda() (switch-to-buffer "*scratch*")))
                (add-hook 'desktop-after-read-hook 'fancy-startup-screen)))))

;; hide the toolbar
(tool-bar-mode 0)
;; (menu-bar-mode 0)

;; set coding system so emacs doesn't choke on melpa file listings
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;;; Install required packages
(require 'cl)

;; set things that need to be set before packages load
; Less crazy key bindings for outline-minor-mode
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
                        ;; melpa packages
                        auctex-latexmk
                        helm
                        helm-descbinds
                        helm-bibtex
                        diminish
                        multi-term
                        anzu
                        howdoi
                        google-this
                        leuven-theme
                        powerline
                        persistent-soft
                        unicode-fonts
                        dired+
                        mouse3
                        outline-magic
                        smooth-scroll
                        company
                        company-math
                        ess
                        markdown-mode
                        polymode
                        eval-in-repl
                        pyvenv
                        anaconda-mode
                        exec-path-from-shell
                        company-anaconda
                        htmlize
                        pcmpl-args
                        pcmpl-pip
                        readline-complete
                        magit
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
  (add-to-list 'fancy-startup-text
               '(:face
                 (variable-pitch default)
                 "Your emacs has been configured for maximum productivity. 
For best results please restart emacs now.
More information about this emacs configuration be found
at http://github.com/izahn/dotemacs. If you have any problems
or have a feature request please open a bug report at
http://github.com/izahn/dotemacs/issues
")))

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

;; finally a theme I can live with!
(load-theme 'leuven t) 
;; but it still needs a few tweeks
(setq org-fontify-whole-heading-line nil)

;; mode line theme
(require 'powerline)
;; face for remote files in modeline
(defface my-mode-line-attention
'((t (:foreground "magenta" :weight bold)))
 "face for calling attention to modeline")

;; highlight hostname if on remote
(defconst my-mode-line-buffer-identification
  '(:eval
    (list
     (propertize
      (if (file-remote-p default-directory 'host)
          (progn
      (let ((host-name
             (or (file-remote-p default-directory 'host)
                 (system-name))))
        (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
            (substring host-name 0 (match-beginning 1))
          host-name)))
        "")
      'face
      (if (file-remote-p default-directory 'host)
          'my-mode-line-attention
        'mode-line-buffer-id))
   (propertize ": %b"
               'face
                 (if (file-remote-p default-directory 'host)
                     'my-mode-line-attention
                   'mode-line-buffer-id)))))

;; powerline theme using above info about remote hosts.
(defun powerline-my-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-raw mode-line-remote nil 'l)
                                     (powerline-raw my-mode-line-buffer-identification nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-my-theme)
(powerline-my-theme)

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

;; enable on-the-fly spell checking
(add-hook 'emacs-startup-hook
          (lambda()
            (add-hook 'text-mode-hook
                      (lambda ()
                        (flyspell-mode 1)))
            ;; prevent flyspell from finding mistakes in the code
            (add-hook 'prog-mode-hook
                      (lambda ()
                        ;; `ispell-comments-and-strings'
                        (flyspell-prog-mode)))))

;; ispell should not check code blocks in org mode
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))

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

;;; Completion hints for files and buffers buffers
(require 'helm-config)
(helm-mode 1)
;; helm global-map
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x b")                        'helm-buffers-list)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-c f")                        'helm-recentf)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h i")                        'helm-info-at-point)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
(global-set-key (kbd "C-c g")                        'helm-gid)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
;; make return do the right thing
(add-hook 'helm-after-initialize-hook
          (lambda()
            ;; complete with enter
            ;; (see http://emacs.stackexchange.com/questions/3798/how-do-i-make-pressing-ret-in-helm-find-files-open-the-directory)
            (defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
              (if (file-directory-p (helm-get-selection))
                  (apply orig-fun args)
                (helm-maybe-exit-minibuffer)))
            (advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
            (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
            ;; backspace deletes whole word
            (defun fu/helm-find-files-navigate-back (orig-fun &rest args)
              (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
                  (helm-find-files-up-one-level 1)
                (apply orig-fun args)))
            (advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)
            ;;; make C-d open dired buffer a-la ido-mode
            ;; first unset C-d
            (define-key helm-find-files-map (kbd "C-d") 'undefined)
            ;; function to open selection in dired
            (defun old-dired (&optional no-op)
              (dired (helm-get-selection)))
            ;; add to helm source 
            (helm-add-action-to-source "Fallback dired"
                                       'old-dired
                                       helm-source-find-files)
            ;; bind to C-d
            (define-key helm-find-files-map (kbd "C-d")
              (lambda () (interactive)
                (helm-quit-and-execute-action 'old-dired)))
            (setq ;; fuzzy match
             helm-recentf-fuzzy-match t
             helm-buffers-fuzzy-matching t
             helm-recentf-fuzzy-match t
             helm-buffers-fuzzy-matching t
             helm-locate-fuzzy-match t
             helm-M-x-fuzzy-match t
             helm-semantic-fuzzy-match t
             helm-imenu-fuzzy-match t
             helm-apropos-fuzzy-match t
             helm-lisp-fuzzy-completion t
             ;; ignore file case (doesn't seem to work, bug?)
             helm-case-fold-search t
             helm-read-file-name-case-fold-search t
             helm-file-name-case-fold-search t
             ;; always display in new buffer below
             helm-always-two-windows t
             ;; don't confuse me with extra instructions
             helm-display-header-line nil)
            ;; less dominating header
            (set-face-attribute 'helm-ff-dotted-directory nil
                                :background nil
                                :foreground "DimGray")
            (set-face-attribute 'helm-source-header nil
                                :background "deep sky blue"
                                :foreground "white"
                                :family "Sans Serif"
                                :height 1.0)
            ;; make helm window smaller
            (helm-autoresize-mode 1)
            (setq helm-autoresize-max-height 30 helm-autoresize-min-height 30)
            ;; learn my usage patterns
            (helm-adaptive-mode 1)))

;;; Helm extras
;; describe active keybindings
(require 'helm-descbinds)
(helm-descbinds-mode)
(require 'helm-bibtex)

;;Use C-TAB to complete. We put this in eval-after-load 
;; because otherwise some modes will try to override our settings.
(require 'company)
;; don't start automatically 
(setq company-idle-delay nil)
;; cancel if input doesn't match
(setq company-require-match nil)
;; complete using C-TAB
(global-set-key (kbd "<C-tab>") 'company-complete)
;; use C-n and C-p to cycle through completions
;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "<tab>") 'company-complete-common)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;; enable math completions
(require 'company-math)
;; company-mode completions for ess
;; (require 'company-ess)
(add-to-list 'company-backends 'company-math-symbols-unicode)
;;(add-to-list 'company-backends 'company-math-symbols-latex)
;; put company-capf at the beginning of the list
(require 'company-capf)
(setq company-backends
      (delete-dups (cons 'company-capf company-backends)))
;; theme
(set-face-attribute 'company-scrollbar-bg nil
                    :background "gray")
(set-face-attribute 'company-scrollbar-fg nil
                    :background "black")
(set-face-attribute 'company-tooltip nil
                    :foreground "black"
                    :background "lightgray")
(set-face-attribute 'company-tooltip-selection nil
                    :foreground "white"
                    :background "steelblue")
;; ;; disable dabbrev
;; (delete 'company-dabbrev company-backends)
;; (delete 'company-dabbrev-code company-backends)


(add-hook 'after-init-hook 'global-company-mode)

;;; Configure outline minor modes
;; Less crazy key bindings for outline-minor-mode
(setq outline-minor-mode-prefix "\C-c\C-o")
;; load outline-magic along with outline-minor-mode
(add-hook 'outline-minor-mode-hook 
          (lambda () 
            (require 'outline-magic)
            (define-key outline-minor-mode-map "\C-c\C-o\t" 'outline-cycle)))

(add-hook 'prog-mode-hook
          (lambda()
            ;; turn on outline minor mode:
            (add-hook 'prog-mode-hook 'outline-minor-mode)
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
          (lambda()
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

;; disable ehoing input
(setq ess-eval-visibly nil)

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

;; truncate long lines in R source files
(add-hook 'ess-mode-hook
          (lambda()
            ;; don't wrap long lines
            (setq truncate-lines 1)
            ;; better (but still not right) indentation
            ;(setq ess-first-continued-statement-offset 2)
            ;(setq ess-continued-statement-offset 0)
            ;(setq ess-arg-function-offset nil)
            ;(setq ess-arg-function-offset-new-line nil)
            ;(setq ess-expression-offset nil)

            ;; ;; put company-capf at the front of the completion sources list
            ;; (set (make-local-variable 'company-backends)
            ;;      (cons 'company-capf company-backends))
            ;; (delete-dups company-backends)
            ))

(when (executable-find "pip")
  (require 'anaconda-mode)
  (require 'company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  (add-hook 'python-mode-hook
            (lambda()
              (setq-local company-backends
                          (cons 'company-anaconda company-backends)))))
;; use ipython if available (but not on windows; see 
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/python.el
;; and only on recent versions of emacs
(when (and (>= (string-to-number 
                (concat 
                 (number-to-string emacs-major-version) 
                 "." 
                 (number-to-string emacs-minor-version)))
               24.4)
           (executable-find "ipython"))
  (unless (eq system-type 'windows-nt)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")))

;; fix printing issue in python buffers
;; see http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21077
(setq python-shell-enable-font-lock nil)

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
          (lambda()
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
          (lambda ()
             (turn-on-reftex)
             (TeX-PDF-mode t)
             (LaTeX-math-mode)
             (TeX-source-correlate-mode t)
             (imenu-add-to-menubar "Index")
             (outline-minor-mode)
             ;; completion
             (setq-local company-backends
                         (delete-dups (cons 'company-files
                                            company-backends)))
             (setq-local company-backends
                         (delete-dups (cons '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode)
                                            company-backends)))
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
          (lambda ()
             (define-key bibtex-mode-map "\M-q" 'bibtex-fill-entry)))

;; enable latexmk
(require 'auctex-latexmk)
(auctex-latexmk-setup)
;; make latexmk the default
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LatexMk")))
;; bad hack to give pdf by default
(unless (file-exists-p "~/.latexmkrc")
    (write-region "# compile to pdf\n$pdf_mode = 1;\n" nil "~/.latexmkrc"))

(require 'org)
(set-face-attribute 'org-meta-line nil
                    :background nil
                    :foreground "#B0B0B0")
(setq org-startup-indented t)
;; increase imenu depth to include third level headings
(setq org-imenu-depth 3)
;; Set sensible mode for editing dot files
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Update images from babel code blocks automatically
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; configure org-mode when opening first org-mode file
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map (kbd "<C-tab>") 'company-complete)
            ;; Load additional export formats
            (require 'ox-odt)
            (require 'ox-md)
            (require 'ox-freemind)
            (require 'ox-bibtex)
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
            ;; Fontify code blocks in org-mode
            (setq org-src-fontify-natively t)
            (setq org-src-tab-acts-natively t)
            (setq org-confirm-babel-evaluate nil)
            (require 'org-capture)
            (require 'org-protocol)
            (require 'ob-stata)
            (when (executable-find "ipython")
              (setq org-babel-python-command
                    "ipython --pylab --pdb --nosep --classic --no-banner --no-confirm-exit")
              ;; https://github.com/jorgenschaefer/elpy/issues/191
              ;; https://lists.gnu.org/archive/html/emacs-orgmode/2014-03/msg00405.html
              ;; make IPython work w/ Org
              (defadvice org-babel-python-evaluate
                  (around org-python-use-cpaste
                          (session body &optional result-type result-params preamble) activate)
                "Add a %cpaste and '--' to the body, so that ipython does the right thing."
                (setq body (concat "%cpaste -q\n" body "\n--\n"))
                ad-do-it
                (if (stringp ad-return-value)
                    (setq ad-return-value
                          (replace-regexp-in-string
                           "\\(^Pasting code; enter '--' alone on the line to stop or use Ctrl-D\.[\r\n]:*\\)"
                           ""
                           ad-return-value)))))))

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
;; show git status in dired
(require 'diff-hl)
(add-hook 'dired-mode-hook 
          (lambda()
            (diff-hl-dired-mode)
            (diff-hl-margin-mode)))

;; show details by default
(setq diredp-hide-details-initially-flag nil)
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
          (lambda()
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

;; term
(require 'multi-term)
(define-key term-mode-map (kbd "C-j") 'term-char-mode)
(define-key term-raw-map (kbd "C-j") 'term-line-mode)
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
  ;; auto-complete for shell-mode (linux only)
(if (eq system-type 'gnu/linux)
    (progn 
      (setq explicit-shell-file-name "bash")
      (setq explicit-bash-args '("-c" "-t" "export EMACS=; stty echo; bash"))  
      (ansi-color-for-comint-mode-on)
      (add-hook 'shell-mode-hook
          (lambda()
             ;; make sure completion calls company-readline first
             (require 'readline-complete)
             (set (make-local-variable 'company-backends)
                  (cons 'company-readline company-backends))
             (delete-dups company-backends)
             ))
      (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))))

(add-hook 'shell-mode-hook
          (lambda()
             ;; add this hook as buffer local, so it runs once per window.
             (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)))

;; extra completion for eshell
(add-hook 'eshell-mode-hook
          (lambda()
             (require 'pcmpl-args)
             (require 'pcmpl-pip)
             ;; programs that don't work well in eshell and should be run in visual mode
             (add-to-list 'eshell-visual-commands "ssh")
             (add-to-list 'eshell-visual-commands "tail")
             (add-to-list 'eshell-visual-commands "htop")
             (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))))

;;; Misc. Conveniences

;; show number of matches in mode line when searching
(global-anzu-mode +1)

;; get help from the web
(require 'google-this)
(google-this-mode 1)
(require 'howdoi)

;; window arrangement history
;; (setq winner-dont-bind-my-keys t) 
(winner-mode 1)

  ;;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)                      
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; ;; use regex search by default
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Make sure copy-and-paste works with other programs
;; (not needed in recent emacs?)
;; (setq x-select-enable-clipboard t
;;       x-select-enable-primary t
;;       save-interprogram-paste-before-kill t)

;; Text pasted with mouse should be inserted at cursor position
(setq mouse-yank-at-point t)

;; Mouse scrolling behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; from https://github.com/bbatsov/prelude
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
`((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

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

(show-paren-mode 1) ;; highlight matching paren

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
          (lambda()
            (setq truncate-lines 1)))

;; don't require two spaces for sentence end.
(setq sentence-end-double-space nil)

;; Use CUA mode only for handy rectangle features
(cua-selection-mode t)

;; use windresize for changing window size
(require 'windresize)

;; use windmove for navigating windows
(global-set-key (kbd "<M-S-left>")  'windmove-left)
(global-set-key (kbd "<M-S-right>") 'windmove-right)
(global-set-key (kbd "<M-S-up>")    'windmove-up)
(global-set-key (kbd "<M-S-down>")  'windmove-down)
;; The beeping can be annoying--turn it off
(set-variable 'visible-bell t)

;; save settings made using the customize interface to a sparate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region ";; Put user configuration here" nil custom-file))
(load custom-file 'noerror)

;; ;; clean up the mode line
(require 'diminish)
;; (diminish 'company-mode)
(diminish 'anzu-mode)
(diminish 'google-this-mode)
(diminish 'outline-minor-mode)
(diminish 'smooth-scroll-mode)
