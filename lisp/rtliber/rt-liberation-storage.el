;;; rt-liberation-storage.el --- Storage backend for rt-liberation

;; Copyright (C) 2010  Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
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

(defvar rt-liber-store nil
  "In memory storage for ticket metadata.")

(defvar rt-liber-store-file "ticket_metadata"
  "File name for storing ticket metadata.")

(defvar rt-liber-ancillary-format-f 'rt-liber-ancillary-format-f
  "Default formatting function for ancillary data.")

(defun rt-liber-store-write ()
  "Write `rt-liber-store' to permanent storage."
  (when (and (stringp rt-liber-directory)
	     (not (file-exists-p rt-liber-directory)))
    (make-directory rt-liber-directory))
  (when (hash-table-p rt-liber-store)
    (with-temp-buffer
      (insert (format "%s" rt-liber-store))
      (insert "\n")
      (write-file
       (concat (file-name-as-directory rt-liber-directory)
	       rt-liber-store-file)))))

(defun rt-liber-store-read ()
  "Populate `rt-liber-store' from permanent storage."
  (let ((file (concat (file-name-as-directory rt-liber-directory)
		      rt-liber-store-file)))
    (when (file-exists-p file)
      (setq rt-liber-store
	    (with-temp-buffer
	      (insert-file-contents file)
	      (read (current-buffer)))))))

(defun rt-liber-store-load ()
  "Ensure that the store has been loaded."
  (when (not (hash-table-p rt-liber-store))
    (rt-liber-store-read)))

(defun rt-liber-store-ticket-property (key value ticket-id)
  "Relate KEY and VALUE to TICKET-ID."
  (rt-liber-store-load)
  (let ((existing (gethash ticket-id rt-liber-store)))
    (when (not existing)
      (setq existing (make-hash-table :test 'equal)))
    (puthash key value existing)
    (puthash ticket-id existing rt-liber-store)
    (rt-liber-store-write)))


;;; ------------------------------------------------------------------
;;; Interface
;;; ------------------------------------------------------------------

(defun rt-liber-set-ancillary-text (text)
  "Store ancillary string TEXT for the ticket at point."
  (let ((id (rt-liber-browser-ticket-id-at-point))
	(inhibit-read-only t)
	message)
    (rt-liber-store-ticket-property "text" text id)
    (if (and rt-liber-ticket-list rt-liber-query)
	(rt-liber-ticketlist-browser-redraw
	 rt-liber-ticket-list rt-liber-query)
      (setq message " (refresh ticket-browser to see changes)"))
    (rt-liber-browser-move-point-to-ticket id)
    (minibuffer-message "stored text for ticket %s%s"
			id (or message ""))))

(defun rt-liber-get-ancillary-text (ticket-id)
  "Return the TEXT field for TICKET-ID or an empty string."
  (rt-liber-store-load)
  (let ((existing (gethash ticket-id rt-liber-store)))
    (if existing
	(or (gethash "text" existing) "")
      "")))

(defun rt-liber-ancillary-format-f (ticket-id)
  "Default formatting function for ancillary data."
  (rt-liber-get-ancillary-text ticket-id))

(defun rt-liber-get-anc (ticket-id)
  "Return formatted ancillary data string for ticket TICKET-ID."
  (funcall rt-liber-ancillary-format-f ticket-id))

(provide 'rt-liberation-storage)

;;; rt-liberation-storage.el ends here.
