;;; rt-liberation-update.el --- check updated tickets

;; Copyright (C) 2009 Yoni Rabkin
;;
;; Authors: Yoni Rabkin <yonirabkin@member.fsf.org>
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

;;; Installation:
;;
;; For installation instructions and detailed help please see the
;; wonderful rt-liberation manual located in the "doc/" directory of
;; the rt-liberation distribution.

;;; Usage:
;;

(require 'rt-liberation)

(defgroup rt-liber-update nil
  "*Check updates for rt-liberation."
  :prefix "rt-liber-update-"
  :group 'rt-liber-update)

(defcustom rt-liber-update-file
  (concat (file-name-as-directory rt-liber-directory) "update")
  "*File to store update information."
  :type 'string
  :group 'rt-liber-update)

(defcustom rt-liber-update-default-queue nil
  "*Default queue to run queries against."
  :type 'string
  :group 'rt-liber-update)

(defun rt-liber-update-timestamp ()
  "Write the current time to disk as a time-stamp."
  (rt-liber-update-write (format "%s" (current-time))))

(defun rt-liber-update-write (str)
  "Write STR to disk under the rt-liber directory."
  (when (and (stringp rt-liber-directory)
	     (not (file-exists-p rt-liber-directory)))
    (make-directory rt-liber-directory))
  (when (stringp rt-liber-update-file)
    (with-temp-buffer
      (insert str)
      (insert "\n")
      (write-file rt-liber-update-file))))

(defun rt-liber-update-read ()
  "Return the time-stamp read from disk or the current time."
  (when (file-exists-p rt-liber-update-file)
    (let ((timestamp (current-time)))
      (with-temp-buffer
	(insert-file-contents rt-liber-update-file)
	(setq timestamp (read (current-buffer))))
      timestamp)))

(defun rt-liber-update-run-query (query-queue timestamp)
  "Run query on QUERY-QUEUE since TIMESTAMP."
  (rt-liber-browse-query
   (rt-liber-compile-query
    (and (queue query-queue)
	 (lastupdated nil
		      (format-time-string "%Y-%m-%d"
					  timestamp))))))

(defun rt-liber-update (&optional no-update)
  "Show tickets last updated since `rt-liber-update' last run.

With the NO-UPDATE prefix, don't update the time-stamp on disk.

If no time-stamp is found, for instance when you run this for the
first time, today's date is used and then recorded as the
time-stamp."
  (interactive "P")
  (when (not rt-liber-update-default-queue)
    (error "`rt-liber-update-default-queue' is nil"))
  (let ((timestamp (rt-liber-update-read)))
    (rt-liber-update-run-query rt-liber-update-default-queue
			       timestamp)
    (when (not no-update)
      (rt-liber-update-timestamp))
    (message "showing tickets updated after: %s (time-stamp %s)"
	     (format-time-string "%Y-%m-%d" timestamp)
	     (if no-update
		 "not updated"
	       "updated"))))

(provide 'rt-liberation-update)

;;; rt-liberation-update.el ends here.
