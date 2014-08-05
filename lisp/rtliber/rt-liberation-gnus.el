;;; rt-liberation-gnus.el --- Gnus integration for rt-liberation

;; Copyright (C) 2009, 2012, 2014 Yoni Rabkin
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

;;; Installation:
;;
;; For installation instructions and detailed help please see the
;; wonderful rt-liberation manual located in the "doc/" directory of
;; the rt-liberation distribution.

(defgroup rt-liber-gnus nil
  "*Gnus integration for rt-liberation."
  :prefix "rt-liber-gnus-"
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-comment-address "no comment address set"
  "*Email address for adding a comment."
  :type 'string
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-address "no reply address set"
  "*Email address for replying to requestor."
  :type 'string
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-subject-name "no subject name set"
  "*Subject name to be included in email header."
  :type 'string
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-answer-headers nil
  "*Alist of additional message headers."
  :type 'list
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-provisional-tag "PROVISIONAL"
  "*Subject line text for a provisional response."
  :type 'string
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-delayed-response-text
  "Please accept my apologies for the late reply."
  "*Text for a belated reply."
  :type 'string
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-signature
  "--
   Regards."
  "Signature to append to email."
  :type 'string
  :group 'rt-liber-gnus)

(defcustom rt-liber-gnus-subject-regexp
  ""
  "Regular expression to capture the ticket number in the subject
line of an email. For example: \\[company.com #\\([0-9].+?\\)\\]"
  :type 'string
  :group 'rt-liber-gnus)

(require 'rt-liberation)
(require 'nnir)
(require 'gnus-msg)


(defun rt-liber-gnus-compose (addr ticket-alist options)
  "Create a Gnus *mail* buffer for the RT email interface.
ADDR email address.
TICKET-ALIST association list of ticket properties.
OPTIONS association list of options.
"
  (let ((provisional      (cdr (assoc 'provisional options)))
	(suppress-subject (cdr (assoc 'suppress-subject options)))
	(top-matter       (cdr (assoc 'top-matter options)))
	(contents         (cdr (assoc 'contents options)))
	(no-comment       (cdr (assoc 'no-comment options))))
    (gnus-setup-message 'message
      (message-mail
       addr
       (format "[%s #%s] %s"
	       rt-liber-gnus-subject-name
	       (rt-liber-ticket-id-only ticket-alist)
	       (cond (suppress-subject "")
		     (provisional rt-liber-gnus-provisional-tag)
		     (t (rt-liber-format "Re: %s" ticket-alist))))
       rt-liber-gnus-answer-headers
       nil
       'switch-to-buffer))
    (goto-char (point-max))
    (save-excursion
      (when top-matter (insert top-matter))
      (let ((start (point)))
	(when contents (newline 2) (insert contents))
	(when (not no-comment)
	  (comment-region start (point))))
      (newline 2)
      (insert rt-liber-gnus-signature)
      (newline))))

(defun rt-liber-gnus-content-to-string ()
  "Return the current content section as a string"
  (rt-liber-gnus-with-ticket-buffer
   (goto-char (point-at-eol))
   (when
       (not
	(or (re-search-backward rt-liber-content-regexp (point-min) t)
	    (re-search-forward rt-liber-content-regexp (point-max) t)))
     (error "no content sections found"))
   (save-excursion
     (goto-char (point-at-bol))
     (re-search-forward "^Content: " (point-at-eol) nil)
     (let ((start (point))
	   text)
       (re-search-forward "^[[:alpha:]]+:" (point-max) t)
       (goto-char (point-at-bol))
       (when (= 0 (length (buffer-substring-no-properties start (point))))
	 (error "empty content section"))
       (setq text (buffer-substring-no-properties start (point)))
       (with-temp-buffer
	 (insert text)
	 (goto-char (point-min))
	 (while (re-search-forward "^[ ]+" (point-max) t)
	   (replace-match ""))
	 (whitespace-cleanup)
	 (setq text (buffer-substring (point-min) (point-max))))
       text))))

(defmacro rt-liber-gnus-with-ticket-buffer (&rest body)
  `(progn
     (when (not (boundp 'rt-liber-ticket-local))
       (error "rt-liberation ticket view buffer not present"))
     ,@body))

(defun rt-liber-gnus-compose-reply-to-requestor ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-address
    rt-liber-ticket-local
    nil)))

(defun rt-liber-gnus-compose-reply-to-requestor-to-this ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-address
    rt-liber-ticket-local
    `((contents . ,(rt-liber-gnus-content-to-string))
      (top-matter . ,(if (rt-liber-ticket-old-p rt-liber-ticket-local)
			 rt-liber-gnus-delayed-response-text
		       nil))))))

(defun rt-liber-gnus-compose-reply-to-requestor-verbatim-this ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-address
    rt-liber-ticket-local
    `((contents . ,(rt-liber-gnus-content-to-string))
      (no-comment . t)))))

(defun rt-liber-gnus-compose-provisional ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-comment-address
    rt-liber-ticket-local
    '((provisional . t)))))

(defun rt-liber-gnus-compose-provisional-to-this ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-comment-address
    rt-liber-ticket-local
    `((provisional . t)
      (contents . ,(rt-liber-gnus-content-to-string))
      (top-matter . ,(if (rt-liber-ticket-old-p rt-liber-ticket-local)
			 rt-liber-gnus-delayed-response-text
		       nil))))))

(defun rt-liber-gnus-compose-comment ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-comment-address
    rt-liber-ticket-local
    `((suppress-subject . t)
      (no-comment       . t)
      ))))

(defun rt-liber-gnus-compose-comment-this ()
  (interactive)
  (rt-liber-gnus-with-ticket-buffer
   (rt-liber-gnus-compose
    rt-liber-gnus-comment-address
    rt-liber-ticket-local
    `((suppress-subject . t)
      (contents . ,(rt-liber-gnus-content-to-string))
      ))))

(defun rt-liber-gnus-visit-ticket-at-point ()
  "Call `rt-liber-display-ticket' on this ticket."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward rt-liber-gnus-subject-regexp (point-at-eol) nil))
  (let ((match (match-string-no-properties 1)))
    (when (not match)
      (error "no ticket number found in subject line"))
    (rt-liber-browse-query
     (rt-liber-compile-query
      (id match)))))

(provide 'rt-liberation-gnus)

;;; rt-liberation-gnus.el ends here.
