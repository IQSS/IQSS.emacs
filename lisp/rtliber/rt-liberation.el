;;; rt-liberation.el --- Free from RT

;; Copyright (C) 2008, 2009, 2010, 2011, 2014 Yoni Rabkin, Aaron
;; S. Hawley, John Sullivan
;;
;; Authors: Yoni Rabkin <yonirabkin@member.fsf.org>, Aaron S. Hawley
;; <aaron.s.hawley@gmail.com>, John Sullivan <johnsu01@wjsullivan.net>
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

;;; Installation and Use:
;;
;; Detailed instructions for installation and use can be found in the
;; rt-liberation manual, in the doc/ directory of the distribution.

;;; History:
;;
;; Started near the end of 2008.

;;; Code:

(require 'browse-url)
(require 'time-date)
(require 'cl)

(defgroup rt-liber nil
  "*rt-liberation, the Emacs interface to RT"
  :prefix "rt-liber-"
  :group 'rt-liber)

(defcustom rt-liber-directory "~/.emacs.d/rt-liber"
  "*Directory to store persistent information."
  :type 'string
  :group 'rt-liber)

(defvar rt-liber-created-string "Created"
  "String representation of \"created\" query tag.")

(defvar rt-liber-base-url ""
  "Base url for ticket display.")

(defvar rt-liber-lastupdated-string "LastUpdated"
  "String representation of \"lastupdated\" query tag.")

(defvar rt-liber-content-string "Content LIKE"
  "String representation of \"content\" query tag.")

(defvar rt-liber-subject-string "Subject LIKE"
  "String representation of \"subject\" query tag.")

(defvar rt-liber-email-address-string "Requestor.EmailAddress LIKE"
  "String representation of \"Requestor.EmailAddress\" query tag.")

(defvar rt-liber-content-not-string "Content NOT LIKE"
  "String representation of \"content\" query tag.")

(defvar rt-liber-subject-not-string "Subject NOT LIKE"
  "String representation of \"subject\" query tag.")

(defvar rt-liber-email-address-not-string "Requestor.EmailAddress NOT LIKE"
  "String representation of \"Requestor.EmailAddress\" query tag.")

(defvar rt-liber-content-regexp "^Content:.*$"
  "Regular expression for section headers.")

(defvar rt-liber-correspondence-regexp
  "^Type: \\(EmailRecord\\|CommentEmailRecord\\|Correspond\\)"
  "Regular expression for correspondence sections.")

(defvar rt-liber-rt-binary "~/rt-3.8.1/bin/rt"
  "Location of the RT CLI binary.")

(defvar rt-liber-rt-version "3.8.1"
  "Version of the RT CLI.")

(defvar rt-liber-username nil
  "Username for assigning ownership on the RT server.")

(defvar rt-liber-browser-buffer-name "*ticket-browser*"
  "Name of ticket browser buffer.")

(defvar rt-liber-browser-buffer nil
  "Ticket browser buffer.")

(defvar rt-liber-browser-default-sorting-function
  'rt-liber-sort-by-time-created
  "Default sorting function.")

(defvar rt-liber-browser-default-filter-function
  'rt-liber-default-filter-f
  "Default filtering function.

This is a function which accepts the ticket alist as a single
argument and returns nil if the ticket needs to be filtered out,
dropped or ignored (however you wish to put it.), otherwise the
function returns a truth value.")

(defvar rt-liber-custom-ticket-redraw-function
  'rt-liber-ticketlist-browser-redraw-f
  "Default ticket redraw function.")

(defvar rt-liber-ticket-old-threshold 30
  "Age in days before a ticket is considered old.")

(defvar rt-liber-jump-to-latest nil
  "jump to the latest correspondence when viewing a ticket.")

(defvar rt-liber-anc-p nil
  "Display ancillary data for tickets.")

(defvar rt-liber-ticket-list nil
  "Ticket-list structure (becomes ticket-browser buffer local).")

(defvar rt-liber-query nil
  "Query structure (becomes ticket-browser buffer local).")

(defvar rt-liber-browser-time-format-string "%b %d %Y %H:%M"
  "String passed to `format-time-string' in the ticket browser.")

(defvar rt-liber-browser-priority-cutoff 0
  "Tickets with a priority higher than this are high priority.")

(defface rt-liber-ticket-face
  '((((class color) (background dark))
     (:foreground "DarkSeaGreen"))
    (((class color) (background light))
     (:foreground "Blue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Blue")))
  "Face for tickets in browser buffer.")

(defface rt-liber-priority-ticket-face
  '((((class color) (background dark))
     (:foreground "Orange"))
    (((class color) (background light))
     (:foreground "Orange"))
    (((type tty) (class mono))
     (:inverse-video t))
    (t (:background "Black")))
  "Face for high priority tickets in browser buffer.")

(defconst rt-liber-viewer-font-lock-keywords
  (let ((header-regexp (regexp-opt '("id: " "Ticket: " "TimeTaken: "
				     "Type: " "Field: " "OldValue: "
				     "NewValue: " "Data: "
				     "Description: " "Created: "
				     "Creator: " "Attachments: ") t)))
    (list
     (list (concat "^" header-regexp ".*$") 0
	   'font-lock-comment-face)))
  "Expressions to font-lock for RT ticket viewer.")

(defvar rt-liber-browser-do-refresh t
  "When t, run `rt-liber-browser-refresh' otherwise disable it.")

(defvar rt-liber-command-dictionary
  '((comment . "comment")
    (edit    . "edit"))
  "Mapping between command symbols and command strings.

The command symbols provide the programmer with a consistent way
of referring to certain commands. The command strings are the
specific strings which would produce the desired effect in the
server.")

(defvar rt-liber-status-dictionary
  '((deleted  . "deleted")
    (resolved . "resolved")
    (open     . "open"))
  "Mapping between status symbols and status strings.

The status symbols provide the programmer with a consistent way
of referring to certain statuses. The status strings are the
server specific strings.")

(defvar rt-liber-custom-field-dictionary
  '((cf-is-spam  . "cf-is-spam"))
  "Mapping between custom field symbols and custom field strings.

The custom field symbols provide the programmer with a consistent
way of referring to certain custom fields. The custom field
strings are the server specific strings.")

(defvar rt-liber-debug-log-enable nil
  "If t then enable logging of communication to a buffer.

Careful! This might create a sizable buffer.")

(defvar rt-liber-debug-log-buffer-name "*rt-liber debug log*"
  "Name of debug log buffer.")

(defvar rt-liber-ticket-local nil
  "Buffer local storage for a ticket.

This variable is made buffer local for the ticket history")

(defvar rt-liber-assoc-browser nil
  "Browser associated with a ticket history.

This variable is made buffer local for the ticket history")

;;; --------------------------------------------------------
;;; Debug log
;;; --------------------------------------------------------

(defun rt-liber-debug-log-write (str)
  "Write STR to debug log."
  (when (not (stringp str))
    (error "must be a string"))
  (with-current-buffer (get-buffer-create
			rt-liber-debug-log-buffer-name)
    (goto-char (point-max))
    (insert str)))


;;; --------------------------------------------------------
;;; TicketSQL compiler
;;; --------------------------------------------------------

(defun rt-liber-bool-p (sym)
  "Return t if SYM is a boolean operator, otherwise nil."
  (member sym '(and or)))
(defun rt-liber-attrib-p (sym)
  "Return t if SYM is a ticket attribute, otherwise nil."
  (member sym '(id owner status subject content queue lastupdatedby
		   email-address)))
(defun rt-liber-time-p (sym)
  "Return t if SYM is a temporal attribute, otherwise nil."
  (member sym '(created lastupdated)))
(defun rt-liber-negation-p (sym)
  (member sym '(not)))

(defun rt-liber-reduce (op seq)
  "Reduce-OP with SEQ to a string of \"s0 op s1 op s2..\"."
  (if seq
      (reduce
       #'(lambda (a b)
	   (format "%s %s %s" a op b))
       seq)
    ""))

(defun rt-liber-make-interval (pred before after)
  "Return a formatted TicketSQL interval.
PRED   temporal attribute predicate.
BEFORE date before predicate.
AFTER  date after predicate."
  (when (string= before "") (setq before nil))
  (when (string= after "") (setq after nil))
  (concat
   (if before (format "%s < '%s'" pred before) "")
   (if (and before after) (format " AND ") "")
   (if after (format "%s > '%s'" pred after) "")))

(defmacro rt-liber-compile-query (query &optional n)
  "Compile sexp-based QUERY into TicketSQL."
  (cond ((null query) `"")
	((stringp query) `,query)
	((rt-liber-bool-p query) `,(upcase (format "%s" query)))
	;; attribute (positive)
	((and (rt-liber-attrib-p query)
	      (not n))
	 `,(cond ((equal query 'content) rt-liber-content-string)
		 ((equal query 'subject) rt-liber-subject-string)
		 ((equal query 'email-address) rt-liber-email-address-string)
		 (t (capitalize (format "%s =" query)))))
	;; attribute (negation)
	((and (rt-liber-attrib-p query)
	      n)
	 `,(cond ((equal query 'content) rt-liber-content-not-string)
		 ((equal query 'subject) rt-liber-subject-not-string)
		 ((equal query 'email-address) rt-liber-email-address-not-string)
		 (t (capitalize (format "%s !=" query)))))
	;; time
	((rt-liber-time-p query)
	 `,(cond ((equal query 'created) rt-liber-created-string)
		 ((equal query 'lastupdated) rt-liber-lastupdated-string)))
	((and (listp query)
	      (rt-liber-time-p (car query)))
	 `(rt-liber-make-interval
	   (rt-liber-compile-query ,(car query))
	   (rt-liber-compile-query ,(cadr query))
	   (rt-liber-compile-query ,(caddr query))))
	;; function (known at compile time?)
	((and query
	      (listp query)
	      (not (rt-liber-bool-p (car query)))
	      (not (rt-liber-negation-p (car query)))
	      (functionp (car query)))
	 `(format "%s" ,query))
	;; negation attribute pairs
	((and (listp query)
	      (rt-liber-negation-p (car query))
	      (rt-liber-attrib-p (caadr query)))
	 `(format "%s '%s'"
		  (rt-liber-compile-query ,(caadr query) t) ; negate
		  (rt-liber-compile-query ,(cadadr query))))
	;; attribute pairs
	((and (listp query)
	      (rt-liber-attrib-p (car query)))
	 `(format "%s '%s'"
		  (rt-liber-compile-query ,(car query))
		  (rt-liber-compile-query ,(cadr query))))
	;; splice boolean operators
	((and (listp query)
	      (rt-liber-bool-p (car query)))
	 `(rt-liber-reduce (rt-liber-compile-query ,(car query))
			   (rt-liber-compile-query ,(cdr query))))
	;; compound statements
	((and (listp query)
	      (not (cdr query)))
	 `(list (rt-liber-compile-query ,(car query))))
	((listp query)
	 `(append
	   (list (rt-liber-compile-query ,(car query)))
	   (rt-liber-compile-query ,(cdr query))))
	;; free variable
	((and query
	      (symbolp query))
	 `(format "%s" ,query))
	(t (error "cannot compile query %s" query))))


;;; --------------------------------------------------------
;;; Query runner
;;; --------------------------------------------------------

(defun rt-liber-query-runner (op query-string)
  "Run OP query against the server with QUERY-STRING."
  (message "started '%s' query at %s..." op (current-time-string))
  (condition-case excep
      (with-temp-buffer
	(if (and (not (rt-liber-version-< rt-liber-rt-version
					  "3.8.2"))
		 (string= op "show"))
	    (call-process rt-liber-rt-binary nil t nil
			  op "-l" query-string)
	  (call-process rt-liber-rt-binary nil t nil
			op query-string))
	(message "query ended at %s" (current-time-string))
	(buffer-string))
    (file-error
     (error "could not find the RT binary at: %s" rt-liber-rt-binary))
    (error "an unhandled exception occured: %s" excep)))

(defun rt-liber-parse-answer (answer-string parser-f)
  "Operate on ANSWER-STRING with PARSER-F."
  (with-temp-buffer
    (insert answer-string)
    (goto-char (point-min))
    (when rt-liber-debug-log-enable
      (rt-liber-debug-log-write (buffer-substring (point-min)
						  (point-max))))
    (funcall parser-f)))


;;; --------------------------------------------------------
;;; TicketSQL runner
;;; --------------------------------------------------------

(defun rt-liber-ticketsql-runner-parser-f ()
  "Parser function for a textual list of tickets."
  (let (idsub-list)
    (while (or
	    (and (not (rt-liber-version-< rt-liber-rt-version
					  "3.8.2"))
		 (re-search-forward "^ *\\([0-9]+\\) *\\(.*\\)$"
				    (point-max) t))
	    (re-search-forward "^\\([0-9]+\\): \\(.*\\)$"
			       (point-max) t))
      (push (list (match-string-no-properties 1)
		  (match-string-no-properties 2))
	    idsub-list))
    idsub-list))

(defun rt-liber-run-ls-query (query)
  "Run an \"ls\" type query against the server with QUERY."
  (rt-liber-parse-answer
   (rt-liber-query-runner "ls" query)
   'rt-liber-ticketsql-runner-parser-f))


;;; --------------------------------------------------------
;;; Ticket list retriever
;;; --------------------------------------------------------

(put 'rt-liber-no-result-from-query-error
     'error-conditions
     '(error rt-liber-errors rt-liber-no-result-from-query-error))

(put 'rt-liber-no-result-from-query-error
     'error-message
     "No results from query")

(defun rt-liber-ticket-base-retriever-parser-f ()
  "Parser function for ticket list."
  (let (ticketbase-list ticketbase (continue t))
    (while (save-excursion
	     (re-search-forward "[A-Za-z]" (point-max) t)) ; really?
      (while (and continue
		  (re-search-forward
		   "^\\(\\([\.{} #[:alpha:]]+\\): \\(.*\\)\\)$\\|^--$"
		   (point-max) t))
	(if (string= (match-string-no-properties 0) "--")
	    (setq continue nil)
	  (push (cons (match-string-no-properties 2)
		      (match-string-no-properties 3))
		ticketbase)))
      (push (copy-seq ticketbase) ticketbase-list)
      (setq ticketbase nil
	    continue t))
    ticketbase-list))

;; accept the output of `rt-liber-ticketsql-runner-parser-f' and
;; return a string suitable for an RT "show" query
(defun rt-liber-create-tickets-string (idsublist)
  "Create a RT CLI ticket \"show\" string from IDSUBLIST."
  (let ((ticket-list (mapcar #'(lambda (e) (car e)) idsublist)))
    (if ticket-list
	(concat "ticket/"
		(if (= (length ticket-list) 1)
		    (format "%s" (car ticket-list))
		  (reduce
		   #'(lambda (a b)
		       (format "%s,%s" a b))
		   ticket-list)))
      (signal 'rt-liber-no-result-from-query-error nil))))

(defun rt-liber-run-show-base-query (idsublist)
  "Run \"show\" type query against the server with IDSUBLIST."
  (rt-liber-parse-answer
   (rt-liber-query-runner "show"
			  (rt-liber-create-tickets-string idsublist))
   #'rt-liber-ticket-base-retriever-parser-f))


;;; --------------------------------------------------------
;;; Ticket retriever
;;; --------------------------------------------------------

;; Implementation note: Working with 3.8.1 or 3.8.2 of the RT CLI
;; makes a huge difference here. 3.8.1 returns a kind of listing of
;; the history ID objects which requires further processing. 3.8.2 on
;; the other hand returns the contents of all the ticket history
;; objects in one fell swoop.

(defun rt-liber-create-ticket-history-string (ticket-id)
  "Create a query for TICKET-ID to retrieve all history objects."
  (concat "ticket/" ticket-id "/history/id"))

(defun rt-liber-create-ticket-histories-string (ticket-id subid-list)
  "Create query for TICKET-ID to retrieve SUBID-LIST objects."
  (concat "ticket/" ticket-id "/history/id/"
	  (reduce
	   #'(lambda (a b) (format "%s,%s" a b)) subid-list)))

(defun rt-liber-run-ticket-history-base-query (ticket-id)
  "Run history query against server for TICKET-ID."
  (rt-liber-parse-answer
   (rt-liber-query-runner "show"
			  (rt-liber-create-ticket-history-string
			   ticket-id))
   (if (rt-liber-version-< rt-liber-rt-version "3.8.2")
       #'(lambda ()
	   (let ((ticket-history-sublist nil))
	     (goto-char (point-min))
	     (while (re-search-forward "^\\([0-9]+\\): " (point-max) t)
	       (setq ticket-history-sublist
		     (append (list (match-string-no-properties 1))
			     ticket-history-sublist)))
	     (if ticket-history-sublist
		 (rt-liber-parse-answer
		  (rt-liber-query-runner
		   "show"
		   (rt-liber-create-ticket-histories-string
		    ticket-id
		    ticket-history-sublist))
		  #'(lambda () (buffer-substring (point-min)
						 (point-max))))
	       (error "an unhandled exceptions occurred"))))
     #'(lambda () (buffer-substring (point-min) (point-max))))))


;;; --------------------------------------------------------
;;; Ticket utilities
;;; --------------------------------------------------------

(defun rt-liber-ticket-days-old (ticket-alist)
  "Return the age of the ticket in positive days."
  (days-between (format-time-string "%Y-%m-%dT%T%z" (current-time))
		(cdr (assoc "Created" ticket-alist))))

(defun rt-liber-ticket-old-p (ticket-alist)
  (<= rt-liber-ticket-old-threshold
      (rt-liber-ticket-days-old ticket-alist)))


;;; --------------------------------------------------------
;;; Ticket viewer
;;; --------------------------------------------------------

(defun rt-liber-jump-to-latest-correspondence ()
  "Move point to the newest correspondence section."
  (interactive)
  (let (latest-point)
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward rt-liber-correspondence-regexp
				(point-min) t)
	(setq latest-point (point))))
    (if latest-point
	(progn
	  (goto-char latest-point)
	  (rt-liber-next-section-in-viewer))
      (message "no correspondence found"))))

(defun rt-liber-ticket-id-only (ticket-alist)
  "Return numerical portion of ticket number from TICKET-ALIST."
  (if ticket-alist
      (substring (cdr (assoc "id" ticket-alist)) 7)
    nil))

(defun rt-liber-ticket-priority-only (ticket-alist)
  "Return an integer value priority or NIL."
  (if ticket-alist
      (let ((p-str (cdr (assoc "Priority" ticket-alist))))
	(if p-str
	    (string-to-number p-str)
	  nil))
    nil))

(defun rt-liber-viewer-visit-in-browser ()
  "Visit this ticket in the RT Web interface."
  (interactive)
  (let ((id (rt-liber-ticket-id-only rt-liber-ticket-local)))
    (if id
	(browse-url
	 (concat rt-liber-base-url "Ticket/Display.html?id=" id))
      (error "no ticket currently in view"))))

(defun rt-liber-viewer-mode-quit ()
  "Bury the ticket viewer."
  (interactive)
  (bury-buffer))

(defun rt-liber-viewer-show-ticket-browser ()
  "Return to the ticket browser buffer."
  (interactive)
  (let ((id (rt-liber-ticket-id-only rt-liber-ticket-local)))
    (if id
	(let ((target-buffer
	       (if rt-liber-assoc-browser
		   (buffer-name rt-liber-assoc-browser)
		 (buffer-name rt-liber-browser-buffer-name))))
	  (if target-buffer
	      (switch-to-buffer target-buffer)
	    (error "associated ticket browser buffer no longer exists"))
	  (rt-liber-browser-move-point-to-ticket id))
      (error "no ticket currently in view"))))

(defun rt-liber-next-section-in-viewer ()
  "Move point to next section."
  (interactive)
  (forward-line 1)
  (when (not (re-search-forward rt-liber-content-regexp (point-max) t))
    (message "no next section"))
  (goto-char (point-at-bol)))

(defun rt-liber-previous-section-in-viewer ()
  "Move point to previous section."
  (interactive)
  (forward-line -1)
  (when (not (re-search-backward rt-liber-content-regexp (point-min) t))
    (message "no previous section"))
  (goto-char (point-at-bol)))

(defconst rt-liber-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'rt-liber-viewer-mode-quit)
    (define-key map (kbd "n") 'rt-liber-next-section-in-viewer)
    (define-key map (kbd "N") 'rt-liber-jump-to-latest-correspondence)
    (define-key map (kbd "p") 'rt-liber-previous-section-in-viewer)
    (define-key map (kbd "V") 'rt-liber-viewer-visit-in-browser)
    (define-key map (kbd "m") 'rt-liber-viewer-answer)
    (define-key map (kbd "M") 'rt-liber-viewer-answer-this)
    (define-key map (kbd "t") 'rt-liber-viewer-answer-provisionally)
    (define-key map (kbd "T") 'rt-liber-viewer-answer-provisionally-this)
    (define-key map (kbd "F") 'rt-liber-viewer-answer-verbatim-this)
    (define-key map (kbd "c") 'rt-liber-viewer-comment)
    (define-key map (kbd "C") 'rt-liber-viewer-comment-this)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "h") 'rt-liber-viewer-show-ticket-browser)
    map)
  "Key map for ticket viewer.")

(define-derived-mode rt-liber-viewer-mode nil
  "RT Liberation Viewer"
  "Major Mode for viewing RT tickets.
\\{rt-liber-viewer-mode-map}"
  (set
   (make-local-variable 'font-lock-defaults)
   '((rt-liber-viewer-font-lock-keywords)))
  (set (make-local-variable 'revert-buffer-function)
       'rt-liber-refresh-ticket-history)
  (set (make-local-variable 'buffer-stale-function)
       (lambda (&optional noconfirm) 'slow))
  (when rt-liber-jump-to-latest
    (rt-liber-jump-to-latest-correspondence))
  (run-hooks 'rt-liber-viewer-hook))

(defun rt-liber-display-ticket-history (ticket-alist &optional assoc-browser)
  "Display history for ticket.

TICKET-ALIST alist of ticket data.
ASSOC-BROWSER if non-nil should be a ticket browser."
  (let* ((ticket-id (rt-liber-ticket-id-only ticket-alist))
	 (contents (rt-liber-run-ticket-history-base-query ticket-id))
	 (new-ticket-buffer (get-buffer-create
			     (concat "*RT Ticket #" ticket-id "*"))))
    (with-current-buffer new-ticket-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert contents)
	(goto-char (point-min))
	(rt-liber-viewer-mode)
	(set
	 (make-local-variable 'rt-liber-ticket-local)
	 ticket-alist)
	(when assoc-browser
	  (set
	   (make-local-variable 'rt-liber-assoc-browser)
	   assoc-browser))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)))
    (switch-to-buffer new-ticket-buffer)))

(defun rt-liber-refresh-ticket-history (&optional ignore-auto noconfirm)
  (interactive)
  (if rt-liber-ticket-local
      (rt-liber-display-ticket-history rt-liber-ticket-local
				       rt-liber-assoc-browser)
    (error "not viewing a ticket")))

;; wrapper functions around specific functions provided by a backend

(declare-function
 rt-liber-gnus-compose-reply-to-requestor
 "rt-liberation-gnus.el")
(declare-function
 rt-liber-gnus-compose-reply-to-requestor-to-this
 "rt-liberation-gnus.el")
(declare-function
 rt-liber-gnus-compose-reply-to-requestor-verbatim-this
 "rt-liberation-gnus.el")
(declare-function
 rt-liber-gnus-compose-provisional
 "rt-liberation-gnus.el")
(declare-function
 rt-liber-gnus-compose-provisional-to-this
 "rt-liberation-gnus.el")
(declare-function
 rt-liber-gnus-compose-comment
 "rt-liberation-gnus.el")
(declare-function
 rt-liber-gnus-compose-comment-this
 "rt-liberation-gnus.el")

(defun rt-liber-viewer-answer ()
  "Answer the ticket."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-reply-to-requestor))
	(t (error "no function defined"))))

(defun rt-liber-viewer-answer-this ()
  "Answer the ticket using the current context."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-reply-to-requestor-to-this))
	(t (error "no function defined"))))

(defun rt-liber-viewer-answer-verbatim-this ()
  "Answer the ticket using the current context verbatim."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-reply-to-requestor-verbatim-this))
	(t (error "no function defined"))))

(defun rt-liber-viewer-answer-provisionally ()
  "Provisionally answer the ticket."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-provisional))
	(t (error "no function defined"))))

(defun rt-liber-viewer-answer-provisionally-this ()
  "Provisionally answer the ticket using the current context."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-provisional-to-this))
	(t (error "no function defined"))))

(defun rt-liber-viewer-comment ()
  "Comment on the ticket."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-comment))
	(t (error "no function defined"))))

(defun rt-liber-viewer-comment-this ()
  "Comment on the ticket using the current context."
  (interactive)
  (cond ((featurep 'rt-liberation-gnus)
	 (rt-liber-gnus-compose-comment-this))
	(t (error "no function defined"))))


;;; --------------------------------------------------------
;;; Ticket browser
;;; --------------------------------------------------------

(declare-function
 rt-liber-get-ancillary-text
 "rt-liberation-storage.el")

;; accept a ticket-alist object and return an alist mapping ticket
;; properties to format characters for use in `rt-liber-format'.
(defun rt-liber-format-function (ticket-alist)
  "Return a pairing of TICKET-ALIST values to %-sequences."
  (let* ((id         (rt-liber-ticket-id-only ticket-alist))
	 (subject    (cdr (assoc "Subject" ticket-alist)))
	 (status     (cdr (assoc "Status" ticket-alist)))
	 (created    (format-time-string
		      rt-liber-browser-time-format-string
		      (date-to-time
		       (cdr (assoc "Created" ticket-alist)))))
	 (resolved   (cdr (assoc "Resolved" ticket-alist)))
	 (requestors (cdr (assoc "Requestors" ticket-alist)))
	 (creator    (cdr (assoc "Creator" ticket-alist)))
	 (owner      (cdr (assoc "Owner" ticket-alist)))
	 (queue      (cdr (assoc "Queue" ticket-alist)))
	 (anc        (if rt-liber-anc-p
			 (rt-liber-get-ancillary-text
			  (rt-liber-ticket-id-only ticket-alist))
		       ""))
	 (priority   (cdr (assoc "Priority" ticket-alist))))
    (list (cons ?i (or id "N/A"))
	  (cons ?s (or subject "N/A"))
	  (cons ?c (or created "N/A"))
	  (cons ?S (or status "N/A"))
	  (cons ?r (or resolved "N/A"))
	  (cons ?R (or requestors "N/A"))
	  (cons ?C (or creator "N/A"))
	  (cons ?o (or owner "N/A"))
	  (cons ?q (or queue "N/A"))
	  (cons ?A (or anc ""))
	  (cons ?p (or priority "N/A")))))

(defun rt-liber-browser-assoc (char alist)
  "Process the %-sequence association."
  (let ((v (cdr (assoc char alist))))
    (cond ((eq char ?%) "%") ;; escape sequence for %
	  (t (or v "")))))

(defun rt-liber-high-priority-p (ticket-alist)
  "Return t if TICKET-ALIST is high priority.

The ticket's priority is compared to the variable
  `rt-liber-browser-priority-cutoff'."
  (let ((p (rt-liber-ticket-priority-only ticket-alist)))
    (if p
	(< rt-liber-browser-priority-cutoff p)
      nil)))

(defun rt-liber-format (format ticket-alist)
  "Substitute %-sequences in FORMAT."
  (let ((alist (rt-liber-format-function ticket-alist)))
    (replace-regexp-in-string
     "%."
     (lambda (str)
       (rt-liber-browser-assoc (aref str 1) alist))
     format t t)))

(defun rt-liber-ticketlist-browser-redraw-f (ticket)
  "Display TICKET."
  (insert (rt-liber-format "[%c %i %S]" ticket))
  (add-text-properties (point-at-bol)
		       (point-at-eol)
		       '(face rt-liber-ticket-face))
  (when (rt-liber-high-priority-p ticket)
    (let ((p (point)))
      (insert (format " HIGH PRIORITY (%d)"
		      (rt-liber-ticket-priority-only ticket)))
      (add-text-properties p
			   (point-at-eol)
			   '(face rt-liber-priority-ticket-face))))

  (newline)
  (insert (rt-liber-format "  [%o] %R: %s" ticket))
  (let ((p (point)))
    (insert (rt-liber-format "    %A" ticket))
    (add-text-properties p (point)
			 '(face font-lock-comment-face)))
  (newline))

(declare-function rt-liber-ticket-marked-p
		  "rt-liberation-multi.el")

(defun rt-liber-ticketlist-browser-redraw (ticketlist &optional query)
  "Display TICKETLIST. Optionally display QUERY as well."
  (erase-buffer)
  (when query
    (insert (format "Query: %s" query))
    (newline)
    (insert (format "%d tickets" (length ticketlist)))
    (newline))
  (when ticketlist
    (let ((filtered-count 0))
      (newline 2)
      (dolist (ticket
	       (funcall rt-liber-browser-default-sorting-function
			ticketlist))
	;; skip filtered tickets, but count how many have been skipped
	(if (funcall rt-liber-browser-default-filter-function ticket)
	    (progn
	      ;; assumes that rt-liber-ticketlist-browser-redraw-f leaves
	      ;; point at the end of the ticket drawn
	      (let ((start (point)))
		(funcall rt-liber-custom-ticket-redraw-function ticket)
		(add-text-properties start
				     (point)
				     (list 'rt-ticket ticket))
		(when (and (featurep 'rt-liberation-multi)
			   (rt-liber-ticket-marked-p ticket))
		  (add-text-properties start
				       (point)
				       '(face rt-liber-marked-ticket-face))))
	      (newline))
	  (setq filtered-count (1+ filtered-count))))
      (when (< 0 filtered-count)
	(insert (format "%d tickets not shown (filtered)" filtered-count))))))

(defun rt-liber-browser-refresh (&optional ignore-auto noconfirm)
  (interactive)
  (if rt-liber-query
      (when (or rt-liber-browser-do-refresh
		noconfirm)
	;; explicitly pass nil NEW to `rt-liber-browse-query'
	(rt-liber-browse-query rt-liber-query nil))
    (error "no buffer-local query")))

(defun rt-liber-browser-refresh-and-return ()
  (interactive)
  (let ((id (rt-liber-browser-ticket-id-at-point)))
    (rt-liber-browser-refresh)
    (rt-liber-browser-move-point-to-ticket id)))

;; This is just a special case of
;; `rt-liber-browser-ticket-<PROPERTY>-at-point'
(defun rt-liber-browser-ticket-id-at-point (&optional point)
  "Return the ticket id for the ticket at buffer position.

If POINT is nil then called on (point)."
  (when (not point)
    (setq point (point)))
  (let ((value (rt-liber-ticket-id-only
		(get-text-property point 'rt-ticket))))
    (if value
	value
      (error "no such ticket property at point"))))

(defun rt-liber-ticket-taken-p (ticket-alist)
  "Return t if TICKET-ALIST is owned by Nobody."
  (when (not ticket-alist)
    (error "null argument"))
  (let ((owner (cdr (assoc "Owner" ticket-alist))))
    (if (string= owner "Nobody")
	nil
      t)))

(defun rt-liber-next-ticket-in-browser ()
  "Move point to the next ticket."
  (interactive)
  (let ((next (next-single-property-change (point) 'rt-ticket)))
    (when next (goto-char next))))

(defun rt-liber-previous-ticket-in-browser ()
  "Move point to the previous ticket."
  (interactive)
  (let ((prev (previous-single-property-change (point) 'rt-ticket)))
    (when prev (goto-char prev))))

(defun rt-liber-display-ticket-at-point ()
  "Display the contents of the ticket at point."
  (interactive)
  (let ((ticket-alist (get-text-property (point) 'rt-ticket)))
    (rt-liber-display-ticket-history ticket-alist (current-buffer))))

(defun rt-liber-browser-search (id)
  "Return point where ticket with ID is displayed or nil."
  (let ((p nil))
    (save-excursion
      (goto-char (point-min))
      (let ((point-id (rt-liber-ticket-id-only
		       (get-text-property (point) 'rt-ticket))))
	(if
	    ;; (predicate) looks for the exceptional situation
	    (and point-id (string= id point-id))
	    ;; (consequent) we're done
	    (setq p (point))
	  ;; (alternative) continue looking
	  (while (and (not p)
		      (rt-liber-next-ticket-in-browser))
	    (let ((point-id (rt-liber-ticket-id-only
			     (get-text-property (point) 'rt-ticket))))
	      (when (string= id point-id)
		(setq p (point))))))))
    p))

(defun rt-liber-browser-move-point-to-ticket (id)
  "Move point to the beginning of ticket with ID."
  (let ((p (rt-liber-browser-search id)))
    (if p
	(progn
	  (goto-char p)
	  (recenter-top-bottom))
      (error "ticket #%s not found" id))))


;;; --------------------------------------------------------
;;; Ticket browser sorting
;;; --------------------------------------------------------

(defun rt-liber-lex-lessthan-p (a b field)
  "Return t if A is lexicographically less than B in FIELD."
  (let ((field-a (cdr (assoc field a)))
	(field-b (cdr (assoc field b))))
    (if (and field-a field-b)
	(string-lessp field-a field-b)
      (error "\"%s\" is not a valid ticket field" field))))

(defun rt-liber-time-lessthan-p (a b field)
  "Return t if A is chronologically less than B in FIELD."
  (let ((field-a (cdr (assoc field a)))
	(field-b (cdr (assoc field b))))
    (if (and field-a field-b)
	(time-less-p (date-to-time field-a)
		     (date-to-time field-b))
      (error "\"%s\" is not a valid ticket field" field))))

(defun rt-liber-sort-ticket-list (ticket-list sort-f)
  "Return a copy of TICKET-LIST sorted by SORT-F."
  (let ((seq (copy-seq ticket-list)))
    (sort seq sort-f)))

(defun rt-liber-sort-by-owner (ticket-list)
  "Sort TICKET-LIST lexicographically by owner."
  (rt-liber-sort-ticket-list
   ticket-list
   #'(lambda (a b)
       (rt-liber-lex-lessthan-p a b "Owner"))))

(defun rt-liber-sort-by-time-created (ticket-list)
  "Sort TICKET-LIST in reverse chronological order."
  (reverse
   (rt-liber-sort-ticket-list
    ticket-list
    #'(lambda (a b)
	(rt-liber-time-lessthan-p a b "Created")))))

;;; --------------------------------------------------------
;;; Ticket browser filtering
;;; --------------------------------------------------------

;; See the fine manual for example code.

(defun rt-liber-default-filter-f (ticket)
  "The default filtering function for the ticket browser

This function is really a placeholder for user custom functions,
and as such always return t."
  t)

;;; --------------------------------------------------------
;;; Version comparison functions
;;; --------------------------------------------------------

;; rt-liber-version-<: string * string -> t-or-nil
(defun rt-liber-version-< (vnum1 vnum2)
  "Test whehther version number VNUM1 is less than VNUM2.
Arguments must be strings Lisp objects, and not numbers.

Examples:
  (rt-liber-version-< \"1.01\" \"1.11\")
    => t

  (rt-liber-version-< \"1.1\" \"1.0.1\")
    => nil"
  (rt-liber-version-<- (rt-liber-version-value
			(rt-liber-version-read vnum1))
		       (rt-liber-version-value
			(rt-liber-version-read vnum2))))

;; rt-liber-version-read: string -> list string
(defun rt-liber-version-read (str)
  "Tokenize version number STR whenever the syntax class changes.

 Example:
   \"1.043.0-1_=+\" \
==> (\"1\" \".\" \"043\" \".\" \"0\" \"-\" \"1\" \"_=+\")"
  (let ((tokens nil)
	(start 0)
	(re (mapconcat 'identity '("[[:digit:]]+" "[[:punct:]]+") "\\|")))
    (while (and (string-match re (substring str start))
		(> (length str) start))
      (setq tokens (cons (match-string 0 (substring str start)) tokens))
      (setq start (+ start (match-end 0))))
    (if (< start (length str))
	(error "Unknown character: %s" (substring str start (1+ start))))
    (reverse tokens)))

;; rt-liber-version-value: list string -> list number
(defun rt-liber-version-value (tokens)
  "Convert list of TOKENS to a comparable number list."
  (mapcar #'(lambda (tk)
	      (if (string-match "^0+$" tk)
		  1
		(if (string-match "^[[:digit:]]+$" tk)
		    (if (string-match "^0+" tk)
			(1+ (* (string-to-number tk)
			       (expt 10
				     (- (length
					 (match-string 0 tk))))))
		      (1+ (string-to-number tk)))
		  (if (string-match "^[[:punct:]]+$" tk)
		      0
		    ;; else (string-match "[^[:digit:][:punct:]]" tk)
		    -1))))
	  tokens))

;; rt-liber-version-<-: list number -> t-or-nil
(defun rt-liber-version-<- (vals1 vals2)
  "Test whether version representation VALS1 is less than VALS2."
  (if (and (null vals1) (null vals2))
      nil
    (if (null vals2)
	nil
      (if (null vals1)
	  t
	(if (= (car vals1) (car vals2))
	    (rt-liber-version-<- (cdr vals1) (cdr vals2))
	  (if (< (car vals1) (car vals2))
	      t
	    nil))))))


;;; --------------------------------------------------------
;;; Entry points
;;; --------------------------------------------------------

(defun rt-liber-browse-query (query &optional new)
  "Run QUERY against the server and launch the browser.

NEW if non-nil create additional browser buffer. If NEW is a
string then that will be the name of the new buffer."
  (interactive "Mquery: ")
  (condition-case excep
      (rt-liber-browser-startup
       (rt-liber-run-show-base-query
	(rt-liber-run-ls-query query))
       query new)
    (rt-liber-no-result-from-query-error
     (rt-liber-browser-with-message "no results from query"
				    query new))))


;;; --------------------------------------------------------
;;; Major mode definitions
;;; --------------------------------------------------------

(declare-function
 rt-liber-multi-flag-as-spam-and-delete
 "rt-liberation-multi.el")

(defun rt-liber-multi-delete-spam ()
  "Delete marked tickets as spam."
  (interactive)
  (cond ((featurep 'rt-liberation-multi)
	 (when (y-or-n-p "Delete marked tickets as spam? ")
	   (rt-liber-multi-flag-as-spam-and-delete)))
	(t (error "rt-liberation-multi isn't loaded"))))

(defun rt-liber-browser-mode-quit ()
  "Bury the ticket browser."
  (interactive)
  (bury-buffer))

(defconst rt-liber-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'rt-liber-browser-mode-quit)
    (define-key map (kbd "n") 'rt-liber-next-ticket-in-browser)
    (define-key map (kbd "p") 'rt-liber-previous-ticket-in-browser)
    (define-key map (kbd "RET") 'rt-liber-display-ticket-at-point)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "G") 'rt-liber-browser-refresh-and-return)
    (define-key map (kbd "s") 'rt-liber-browser-mark-as-spam)
    (define-key map (kbd "S") 'rt-liber-multi-delete-spam)
    (define-key map (kbd "a") 'rt-liber-browser-assign)
    (define-key map (kbd "r") 'rt-liber-browser-resolve)
    (define-key map (kbd "o") 'rt-liber-browser-open)
    (define-key map (kbd "t") 'rt-liber-browser-take-ticket-at-point)
    (define-key map (kbd "A") 'rt-liber-browser-ancillary-text)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "m") 'rt-liber-browser-move)
    (define-key map (kbd "M") 'rt-liber-mark-ticket-at-point)
    (define-key map (kbd "P") 'rt-liber-browser-prioritize)
    map)
  "Key map for ticket browser.")

(define-derived-mode rt-liber-browser-mode nil
  "RT Liberation Browser"
  "Major Mode for browsing RT tickets.
\\{rt-liber-browser-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       'rt-liber-browser-refresh)
  (set (make-local-variable 'buffer-stale-function)
       (lambda (&optional noconfirm) 'slow))
  (run-hooks 'rt-liber-browser-hook))

(defun rt-liber-setup-browser-name (new)
  (setq rt-liber-browser-buffer
	(get-buffer-create
	 (if new
	     (generate-new-buffer-name
	      (if (stringp new)
		  new
		rt-liber-browser-buffer-name))
	   (if (and (boundp 'rt-liber-query)
		    rt-liber-query)
	       (buffer-name)
	     rt-liber-browser-buffer-name)))))

(defun rt-liber-browser-with-message (message &optional query new)
  "Start the RT ticket browser and display MESSAGE."
  (interactive)
  (rt-liber-setup-browser-name new)
  ;; setup stage (invisible to user)
  (with-current-buffer rt-liber-browser-buffer
    (let ((inhibit-read-only t))
      (rt-liber-browser-mode)
      (goto-char (point-min))
      (rt-liber-ticketlist-browser-redraw nil query)
      (newline 2)
      (insert message)
      (set (make-local-variable 'rt-liber-query) query)))
  ;; display stage (user can see updates)
  (switch-to-buffer rt-liber-browser-buffer)
  (setq buffer-read-only t))

(defun rt-liber-browser-startup (ticket-list &optional query new)
  "Start the RT ticket browser."
  (interactive)
  (rt-liber-setup-browser-name new)
  ;; setup stage (invisible to user)
  (with-current-buffer rt-liber-browser-buffer
    (let ((inhibit-read-only t))
      (rt-liber-ticketlist-browser-redraw ticket-list query)
      (goto-char (point-min))
      (rt-liber-next-ticket-in-browser)
      (rt-liber-browser-mode)
      ;; store the ticket-list and the query which produced the buffer
      ;; as buffer local variables
      (set (make-local-variable 'rt-liber-ticket-list) ticket-list)
      (set (make-local-variable 'rt-liber-query) query)))
  ;; display stage (user can see updates)
  (switch-to-buffer rt-liber-browser-buffer)
  (setq buffer-read-only t))

(declare-function rt-liber-set-ancillary-text "rt-liberation-storage.el")

(defun rt-liber-browser-ancillary-text ()
  "Wrapper function around storage backend."
  (interactive)
  (when (not (featurep 'rt-liberation-storage))
    (error "rt-liberation-storage isn't loaded"))
  (let ((initial-contents ""))
    (rt-liber-set-ancillary-text
     (read-from-minibuffer "Text: " initial-contents))))

;;; --------------------------------------------------------
;;; Command module
;;; --------------------------------------------------------

;; when this module is stable enough it should be documented in the
;; manual -- yrk

(defun rt-liber-command-get-dictionary-value (sym dic)
  "Utility function for retrieving alist values."
  (let ((value (cdr (assoc sym dic))))
    (if value
	value
      (error "%s not a key in dictionary %s" sym dic))))

(defun rt-liber-command-get-command-string (command-symbol)
  "Return value associated with key COMMAND-SYMBOL."
  (rt-liber-command-get-dictionary-value
   command-symbol
   rt-liber-command-dictionary))

(defun rt-liber-command-get-status-string (status-symbol)
  "Return value associated with key STATUS-SYMBOL."
  (rt-liber-command-get-dictionary-value
   status-symbol
   rt-liber-status-dictionary))

(defun rt-liber-command-get-custom-field-string (custom-field-symbol)
  "Return value associated with key CUSTOM-FIELD-SYMBOL."
  (rt-liber-command-get-dictionary-value
   custom-field-symbol
   rt-liber-custom-field-dictionary))

(defun rt-liber-command-runner (op arg-string)
  "Run OP command against the server with ARG-STRING."
  (message "started '%s' command at %s..." op (current-time-string))
  (condition-case excep
      (with-temp-buffer
	(call-process-shell-command rt-liber-rt-binary nil t nil
				    op arg-string)
	(message "command ended at %s" (current-time-string))
	(buffer-string))
    (file-error
     (error "could not find the RT binary at: %s" rt-liber-rt-binary))
    (error "an unhandled exception occured: %s" excep)))

;; the user might not see this but at the very least it will leave a
;; trace in the *Messages* buffer -- yrk
(defun rt-liber-command-runner-parser-f ()
  "Display command return status from the server to the user."
  (message (buffer-string)))

(defun rt-liber-command-set-cf (id field value)
  "Add custom field FIELD with VALUE to ID.
If FIELD already exists, update to VALUE."
  ;; TODO: This may not work with standard rt cli.
  ;;
  ;; works fine with the stock version 3.8.2 -- yrk

  ;; TODO: Should probably bust comment out to its own function.
  (let ((command (rt-liber-command-get-command-string 'comment))
	(args
	 (format "-f %s=%s ticket/%s" field value id)))
    (rt-liber-parse-answer
     (rt-liber-command-runner command args)
     'rt-liber-command-runner-parser-f)))

(defun rt-liber-command-set-status (id status)
  "Set ticket ID status to be STATUS."
  ;; TODO: Sanity check status
  ;; TODO: defmacro?
  (let ((command (rt-liber-command-get-command-string 'edit))
	(args
	 (format "ticket/%s set status=%s" id status)))
    (rt-liber-parse-answer
     (rt-liber-command-runner command args)
     'rt-liber-command-runner-parser-f)))

(defun rt-liber-command-set-priority (id priority)
  "Set ticket ID priority to be PRIORITY."
  (let ((command (rt-liber-command-get-command-string 'edit))
	(args
	 (format "ticket/%s set priority=%s"
		 id
		 (if (= priority 0)
		     ;; this is to work around a weird bug in RT
		     ;; (3.8.8) in which the CLI command to set the
		     ;; priority to 0 doesn't work, but sending 00
		     ;; does
		     "00"
		   (format "%d" priority)))))
    (rt-liber-parse-answer
     (rt-liber-command-runner command args)
     'rt-liber-command-runner-parser-f)))

(defun rt-liber-command-set-status-deleted (id)
  "Set the status of ticket ID to `deleted'."
  (rt-liber-command-set-status
   id (rt-liber-command-get-status-string 'deleted)))

(defun rt-liber-command-set-status-resolved (id)
  "Set the status of ticket ID to `resolved'."
  (rt-liber-command-set-status
   id (rt-liber-command-get-status-string 'resolved)))

(defun rt-liber-command-set-status-open (id)
  "Set the status of ticket ID to `open'."
  (rt-liber-command-set-status
   id (rt-liber-command-get-status-string 'open)))

;; This is brittle because the server doesn't respond with a code but
;; with some free text, and we have no guarantee that the text will be
;; stable from version to version.
(defun rt-liber-handle-response-set-owner (response)
  "Handle the response from the RT server. Pass on the response."
  (when
      (with-temp-buffer
	(insert response)
	(goto-char (point-min))
	(re-search-forward "That user does not exist" (point-max) t))
    (error "that user does not exist"))
  response)

(defun rt-liber-command-set-owner (id owner)
  "Set the owner of ticket ID to OWNER."
  (let ((command (rt-liber-command-get-command-string 'edit))
	(args
	 (format "ticket/%s set owner=%s" id owner)))
    (rt-liber-handle-response-set-owner
     (rt-liber-parse-answer
      (rt-liber-command-runner command args)
      'rt-liber-command-runner-parser-f))))

(defun rt-liber-command-set-queue (id queue)
  "Set the queue of ticket ID to QUEUE."
  (let ((command (rt-liber-command-get-command-string 'edit))
	(args
	 (format "ticket/%s set queue=%s" id queue)))
    (rt-liber-parse-answer
     (rt-liber-command-runner command args)
     'rt-liber-command-runner-parser-f)))

(defun rt-liber-browser-prioritize (n)
  "Assigng current ticket priority N."
  (interactive "nPriority (number): ")
  (rt-liber-command-set-priority
   (rt-liber-browser-ticket-id-at-point)
   n)
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-assign (name)
  "Assign current ticket to a user NAME."
  (interactive "sAssign to: ")
  (let ((taken-p (rt-liber-ticket-taken-p
		  (get-text-property (point) 'rt-ticket))))
    (when (or (not taken-p)
	      (and taken-p
		   (y-or-n-p "Ticket already assigned! Are you sure?")))
      (rt-liber-command-set-owner
       (rt-liber-browser-ticket-id-at-point)
       name)
      (rt-liber-browser-refresh-and-return))))

(defun rt-liber-browser-resolve ()
  "Resolve the current ticket."
  (interactive)
  (rt-liber-command-set-status-resolved
   (rt-liber-browser-ticket-id-at-point))
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-open ()
  "Open the current ticket."
  (interactive)
  (rt-liber-command-set-status-open
   (rt-liber-browser-ticket-id-at-point))
  (rt-liber-browser-refresh-and-return))

(defun rt-liber-browser-move (queue)
  "Move the current ticket to a different queue."
  (interactive "sQueue: ")
  (rt-liber-command-set-queue
   (rt-liber-browser-ticket-id-at-point)
   queue)
  (rt-liber-browser-refresh))

(defun rt-liber-browser-mark-as-spam ()
  "Mark the current ticket as spam, and delete it."
  (interactive)
  (if (y-or-n-p "Delete marked ticket as spam? ")
      (let ((id (rt-liber-browser-ticket-id-at-point)))
	(rt-liber-command-set-cf
	 id (rt-liber-command-get-custom-field-string 'cf-is-spam) "yes")
	(rt-liber-command-set-status-deleted id)
	(rt-liber-browser-refresh))
    (message "aborted")))

(defun rt-liber-browser-take-ticket-at-point ()
  "Assign the ticket under point to `rt-liber-username'."
  (interactive)
  (when (not rt-liber-username)
    (error "`rt-liber-username' is nil"))
  (let ((taken-p (rt-liber-ticket-taken-p
		  (get-text-property (point) 'rt-ticket))))
    (when (or (not taken-p)
	      (and taken-p
		   (y-or-n-p "Ticket already taken! Are you sure?")))
      (rt-liber-command-set-owner
       (rt-liber-browser-ticket-id-at-point)
       rt-liber-username)
      (rt-liber-browser-refresh))))

(defun rt-liber-viewer-take-ticket ()
  "Assign the current ticket to `rt-liber-username'."
  (interactive)
  (when (not rt-liber-username)
    (error "`rt-liber-username' is nil"))
  (let ((id (rt-liber-ticket-id-only rt-liber-ticket-local))
	(taken-p (rt-liber-ticket-taken-p rt-liber-ticket-local)))
    (if id
	(progn
	  (when (or (not taken-p)
		    (and taken-p
			 (y-or-n-p "Ticket already taken! Are you sure?")))
	    (rt-liber-command-set-owner id rt-liber-username)
	    (rt-liber-refresh-ticket-history)))
      (error "no ticket currently in view"))))


(provide 'rt-liberation)

;;; rt-liberation.el ends here.
