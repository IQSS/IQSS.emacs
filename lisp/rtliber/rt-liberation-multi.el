;;; rt-liberation-multi.el --- Free from RT

;; Copyright (C) 2010, 2014 Yoni Rabkin
;;
;; Authors: Yoni Rabkin <yrk@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(require 'rt-liberation)

(defface rt-liber-marked-ticket-face
  '((((class color) (background dark))
     (:foreground "Red"))
    (((class color) (background light))
     (:foreground "Red"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Red")))
  "Face for marked tickets in browser buffer."
  :group 'rt-liberation)

(defvar rt-liber-marked-tickets nil
  "Marked tickets (becomes ticket-browser buffer local).")

(defun rt-liber-ticket-marked-p (ticket)
  "Return a truth value if TICKET is marked, otherwise NIL."
  (let ((ticket-id (rt-liber-ticket-id-only ticket)))
    (member ticket-id rt-liber-marked-tickets)))

(defun rt-liber-mark-ticket-at-point ()
  "Mark/unmark the ticket at point for future action."
  (interactive)
  (let ((ticket (get-text-property (point) 'rt-ticket)))
    (if ticket
	(let ((ticket-id (rt-liber-ticket-id-only ticket))
	      (inhibit-read-only t))
	  (set (make-local-variable 'rt-liber-marked-tickets)
	       (if (rt-liber-ticket-marked-p ticket)
		   (remove ticket-id rt-liber-marked-tickets)
		 (append (list ticket-id) rt-liber-marked-tickets)))
	  (let ((p (point)))
	    (rt-liber-ticketlist-browser-redraw
	     rt-liber-ticket-list
	     rt-liber-query)
	    (goto-char p)
	    (rt-liber-next-ticket-in-browser)
	    (recenter-top-bottom)))
      (error "no ticket at point"))))

(defun rt-liber-multi-set-status-open ()
  "Set the status of all marked tickets to `open'."
  (interactive)
  (when (not rt-liber-marked-tickets)
    (error "no marked tickets"))
  (dolist (ticket-id rt-liber-marked-tickets)
    (message "setting status for ticket #%s ..." ticket-id)
    (rt-liber-command-set-status-open ticket-id)
    (message "setting status for ticket #%s ...done" ticket-id))
  (rt-liber-browser-refresh))

(defun rt-liber-multi-set-status-resolved ()
  "Set the status of all marked tickets to `resolve'."
  (interactive)
  (when (not rt-liber-marked-tickets)
    (error "no marked tickets"))
  (dolist (ticket-id rt-liber-marked-tickets)
    ;; these are less for the user and more to leave a trail in the
    ;; `*Messages*' buffer
    (message "setting status for ticket #%s ..." ticket-id)
    (rt-liber-command-set-status-resolved ticket-id))
  (rt-liber-browser-refresh))

(defun rt-liber-multi-assign (name)
  "Assign marked tickets to a user NAME."
  (interactive "sAssign to: ")
  (when (not rt-liber-marked-tickets)
    (error "no marked tickets"))
  (dolist (ticket-id rt-liber-marked-tickets)
    (message "setting status for ticket #%s ..." ticket-id)
    (rt-liber-command-set-owner
     ticket-id name))
  (rt-liber-browser-refresh))

(defun rt-liber-multi-flag-as-spam-and-delete ()
  "Flag all marked tickets as spam, and delete them."
  (interactive)
  (when (not rt-liber-marked-tickets)
    (error "no marked tickets"))
  (dolist (ticket-id rt-liber-marked-tickets)
    (rt-liber-command-set-cf
     ticket-id
     (rt-liber-command-get-custom-field-string
      'cf-is-spam) "yes")
    (rt-liber-command-set-status-deleted ticket-id))
  (rt-liber-browser-refresh))

(provide 'rt-liberation-multi)

;;; rt-liberation-multi.el ends here.
