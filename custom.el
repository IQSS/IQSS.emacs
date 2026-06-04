;; Put your personal user configuration in this file.

;; To require addional packages add them to 'package-selected-packages, e.g.
;; (add-to-list 'package-selected-packages 'ess)
;; will ensure that the ess package is installed the next time Emacs starts.

;; Don't remove this:
(unless (cl-every 'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

(set-face-attribute 'default nil :family "Monaco" :height 180)

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

;; from Len for copilot
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'quelpa)
;; Don't re-clone the MELPA recipe repo on every launch; reuse the existing
;; checkout. Set back to t (or run M-x quelpa-upgrade-all) to update Copilot.
(setq quelpa-update-melpa-p nil)
(require 'use-package)
(require 'quelpa-use-package)
(use-package copilot
 :quelpa (copilot :fetcher github
          :repo "zerolfx/copilot.el"
          :branch "main"
          :files ("dist" "*.el")))
(define-key copilot-mode-map (kbd "M-C-<return>") #'copilot-accept-completion)
(define-key copilot-mode-map (kbd "C-<right>") #'copilot-accept-completion-by-word)

