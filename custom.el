;; Put your personal user configuration in this file.

;; To require addional packages add them to 'package-selected-packages, e.g.
;; (add-to-list 'package-selected-packages 'ess)
;; will ensure that the ess package is installed the next time Emacs starts.

;; Don't remove this:
(add-to-list 'package-selected-packages 'jinx)
(unless (cl-every 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

(set-face-attribute 'default nil :family "Monaco" :height 180)

;; Jinx: fast Enchant-based spell checker (replaces flyspell from IQSS init).
;; Needs Homebrew `enchant` (+ `aspell` for English). Correct with M-$;
;; C-u M-$ checks the whole buffer. Right-click a wavy underline for suggestions.
(when (require 'jinx nil t)
  ;; IQSS enables flyspell via anonymous hooks; turn it off once it starts.
  (add-hook 'flyspell-mode-hook
            (lambda ()
              (when flyspell-mode
                (flyspell-mode -1))))
  (setq jinx-languages "en")
  (keymap-global-set "M-$" #'jinx-correct)
  (keymap-global-set "C-M-$" #'jinx-languages)
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  ;; If this file is reloaded after startup, enable immediately.
  (when after-init-time
    (global-jinx-mode 1)))

;; Open Emacs at a comfortable default size (the size used on the large
;; monitor), but shrink to fit when the current screen is smaller (e.g.
;; laptop only), and right-justify it (top-right corner of the screen).
;; `frame-monitor-workarea' already excludes the macOS menu bar and Dock,
;; so the window always fits. The frame stays freely resizable.
(defun my/set-default-frame-size (&optional frame)
  "Size FRAME to the preferred default (clamped to its monitor) and right-justify it."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (let* ((pref-w 1053)              ; preferred width  in pixels
             (pref-h 1382)              ; preferred height in pixels
             (work (frame-monitor-workarea frame))
             (work-x (nth 0 work))
             (work-y (nth 1 work))
             (work-w (nth 2 work))
             (work-h (nth 3 work))
             (w (min pref-w work-w))
             (h (min pref-h work-h)))
        (set-frame-size frame w h t)
        ;; Align the frame's right edge with the work area's right edge.
        (set-frame-position frame
                            (+ work-x (max 0 (- work-w (frame-pixel-width frame))))
                            work-y)))))
(my/set-default-frame-size)
(add-hook 'after-make-frame-functions #'my/set-default-frame-size)

;; Map Ctrl + Up and Ctrl + Down to scroll without moving cursor
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)

;; Don't let Cmd+Q (s-q) quit Emacs; use C-x C-c instead.
(global-set-key (kbd "s-q")
                (lambda () (interactive)
                  (message "Cmd+Q is disabled. Use C-x C-c to quit Emacs.")))

;; Magit: C-x g opens the status buffer; refine diffs to word/character level
;; (much easier to read for prose/LaTeX).
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-refine-hunk 'all)

;; AUCTeX SyncTeX / Skim on macOS.
;; C-c C-a (TeX-command-run-all) runs LaTeX → Biber (if needed) → LaTeX → View.
;; Forward search (Emacs → Skim): C-c C-v, or the final View step of C-c C-a.
;; Inverse search (Skim → Emacs): Shift+Cmd+click in Skim.
(when (eq system-type 'darwin)
  (let ((texbin "/Library/TeX/texbin"))
    (when (file-directory-p texbin)
      (add-to-list 'exec-path texbin)
      (setenv "PATH" (concat texbin path-separator (or (getenv "PATH") "")))))
  ;; Emacs server must be running for Skim inverse search.
  (when (and (display-graphic-p) (not (daemonp)))
    (require 'server)
    (unless (server-running-p)
      (server-start)))
  ;; AUCTeX variables only exist after the tex library is loaded.
  (with-eval-after-load "tex"
    (setq TeX-source-correlate-method 'synctex
          TeX-source-correlate-start-server t)
    ;; Use Skim's displayline (synctex) instead of plain "open -a Skim".
    (setq TeX-view-program-selection '((output-pdf "Skim")))
    (add-to-list 'TeX-view-program-list
                 '("Skim"
                   "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"
                   "/Applications/Skim.app/Contents/SharedSupport/displayline")
                 t)))

;; natbib citation commands (\citep, \citet, …) are not in font-latex's
;; built-in reference list; AUCTeX adds them only after async style parsing.
;; Register them up front so they stay citation-colored (not dim gray).
(with-eval-after-load "Latex"
  (defun my/font-latex-natbib-cites ()
    (when (fboundp 'font-latex-add-keywords)
      (font-latex-add-keywords
       '(("citep"        "*[[{") ("citet"        "*[[{")
         ("citep*"       "*[[{") ("citet*"       "*[[{")
         ("citealt"      "*[[{") ("citealt*"     "*[[{")
         ("citealp"      "*[[{") ("citealp*"     "*[[{")
         ("citeauthor"   "*[[{") ("citeauthor*"  "*[[{")
         ("citefullauthor" "[[{") ("citeyear"   "[[{") ("citeyearpar" "[[{")
         ("Citep"        "*[[{") ("Citet"        "*[[{")
         ("Citealt"      "*[[{") ("Citealp"      "*[[{")
         ("Citeauthor"   "*[[{")
         ("citetalias"   "*[[{") ("citepalias"   "*[[{"))
       'reference)))
  (add-hook 'LaTeX-mode-hook #'my/font-latex-natbib-cites))

;; Auto-reload file buffers when something else changes them on disk
;; (e.g. a Cursor agent).  Without this, Emacs keeps the stale in-memory
;; copy until you manually revert.  Unmodified buffers update silently;
;; if you have unsaved edits, Emacs will prompt before discarding them.
(require 'autorevert)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Preserve outline-minor-mode folding across buffer reverts.
;; When an external program (e.g. a Cursor agent) edits a file on disk, Emacs
;; auto-reverts the buffer.  A native revert of an outline-folded LaTeX buffer
;; *over-collapses* it: the body of a high-level heading such as \begin{document}
;; ends up hiding every \section and frame heading nested beneath it, so the
;; document collapses to just a few top-level lines.  We wrap `revert-buffer':
;; capture which headings had folded bodies, let the revert run, then fully
;; expand (to clear the over-collapse) and re-fold those headings.  The captured
;; list lives in a lexical (let) variable so it survives the revert, and the
;; restore runs once immediately and once on a short idle timer to survive
;; AUCTeX's asynchronous style-hook pass.
(defun my/outline-folded-headings ()
  "Return the list of heading lines whose body is currently hidden."
  (when (bound-and-true-p outline-minor-mode)
    (let (folded)
      (save-excursion
        (goto-char (point-min))
        (unless (outline-on-heading-p t)
          (outline-next-heading))
        (while (and (not (eobp)) (outline-on-heading-p t))
          (when (save-excursion
                  (outline-end-of-heading)
                  (outline-invisible-p (point)))
            (push (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))
                  folded))
          (outline-next-heading)))
      (nreverse folded))))

(defun my/outline-apply-folds (headings)
  "Hide the body of each heading in HEADINGS, matched by heading text."
  (when (and headings (bound-and-true-p outline-minor-mode))
    (let ((remaining (copy-sequence headings)))
      (save-excursion
        (goto-char (point-min))
        (unless (outline-on-heading-p t)
          (outline-next-heading))
        (while (and (not (eobp)) (outline-on-heading-p t) remaining)
          (let ((text (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (member text remaining)
              (setq remaining (delete text remaining))
              (outline-hide-entry)))
          (outline-next-heading))))))

(defun my/outline-restore-overview (folds buf)
  "Re-establish the outline overview described by FOLDS in BUF.
A native `revert-buffer' of an outline-folded LaTeX buffer *over-collapses* it:
the body of a high-level heading such as \\begin{document} ends up hiding every
section and frame heading nested beneath it, so the buffer collapses to just a
few top-level lines.  Simply re-hiding bodies cannot undo that, because the
section headings themselves are invisible.  So we first fully expand the buffer
to clear the over-collapse, then re-hide exactly the bodies that were folded
before the revert."
  (when (and folds (buffer-live-p buf) (bound-and-true-p outline-minor-mode))
    (outline-show-all)
    (my/outline-apply-folds folds)
    (force-window-update buf)))

(define-advice revert-buffer
    (:around (orig &rest args) my/preserve-outline-folds)
  "Keep outline-minor-mode folding across a revert (e.g. external edits).
A native revert over-collapses an outline-folded LaTeX buffer (hiding even the
section headings), and AUCTeX re-applies its style hooks asynchronously after
the revert returns.  We capture which headings had folded bodies, run the
revert, then restore the overview (expand to undo the over-collapse, re-fold
those headings, repaint) -- once immediately and once on a short idle timer to
survive the async style-hook pass."
  (if (not (bound-and-true-p outline-minor-mode))
      (apply orig args)
    (let ((folds (my/outline-folded-headings))
          (buf (current-buffer)))
      (apply orig args)
      (my/outline-restore-overview folds buf)
      (run-at-time 0.15 nil
                   (lambda (b f)
                     (when (buffer-live-p b)
                       (with-current-buffer b
                         (my/outline-restore-overview f b))))
                   buf folds))))

;; from Len for copilot
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; Copilot is installed through quelpa.  Only set it up when quelpa is actually
;; installed, so a machine without it still starts cleanly instead of aborting
;; here (which would skip the rest of this file and the bindings in init.el).
;; To enable Copilot on such a machine: M-x package-install RET quelpa
(when (require 'quelpa nil t)
  ;; Don't re-clone the MELPA recipe repo on every launch; reuse the existing
  ;; checkout. Set back to t (or run M-x quelpa-upgrade-all) to update Copilot.
  (setq quelpa-update-melpa-p nil)
  (quelpa '(copilot :fetcher github
                    :repo "zerolfx/copilot.el"
                    :branch "main"
                    :files ("dist" "*.el")))
  (when (require 'copilot nil t)
    (define-key copilot-mode-map (kbd "M-C-<return>") #'copilot-accept-completion)
    (define-key copilot-mode-map (kbd "C-<right>") #'copilot-accept-completion-by-word)))

