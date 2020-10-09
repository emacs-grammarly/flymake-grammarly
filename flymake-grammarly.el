;;; flymake-grammarly.el --- Flymake support for Grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-08-23 17:21:13

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Flymake support for Grammarly.
;; Keyword: grammar check
;; Version: 0.1.3
;; Package-Requires: ((emacs "26.1") (grammarly "0.1.3"))
;; URL: https://github.com/jcs-elpa/flymake-grammarly

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flymake support for Grammarly.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'dom)

(require 'flymake)
(require 'grammarly)

(defgroup flymake-grammarly nil
  "Flymake support for Grammarly."
  :prefix "flymake-grammarly-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/jcs-elpa/flymake-grammarly"))

(defcustom flymake-grammarly-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of mode will enable grammarly in the buffer."
  :type 'list
  :group 'flymake-grammarly)

(defcustom flymake-grammarly-check-time 0.8
  "How long do we call request after we done typing."
  :type 'float
  :group 'flymake-grammarly)

(defvar flymake-grammarly--show-debug-message nil
  "Show the debug message from this package.")

(defvar-local flymake-grammarly--done-checking nil
  "Check if Grammarly API done checking.")

(defvar-local flymake-grammarly--point-data '()
  "List of error/warning JSON data.")

(defvar-local flymake-grammarly--last-buffer-string nil
  "Record the last buffer string.")

(defvar-local flymake-grammarly--request-timer nil
  "Timer that will tell to do the request.")

;;; Util

(defun flymake-grammarly--column-at-pos (&optional pt)
  "Column at PT."
  (unless pt (setq pt (point)))
  (save-excursion (goto-char pt) (current-column)))

(defun flymake-grammarly--debug-message (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when flymake-grammarly--show-debug-message
    (apply #'message fmt args)))

;;; Grammarly

(defun flymake-grammarly--on-open ()
  "On open Grammarly API."
  (when flymake-mode
    (flymake-grammarly--debug-message "[INFO] Start connecting to Grammarly API...")))

(defun flymake-grammarly--on-message (data)
  "Received DATA from Grammarly API."
  (when flymake-mode
    (flymake-grammarly--debug-message "[INFO] Receiving data from grammarly, level (%s)" (length flymake-grammarly--point-data))
    (when (string-match-p "\"point\":" data)
      (push data flymake-grammarly--point-data))))

(defun flymake-grammarly--on-close ()
  "On close Grammarly API."
  (when flymake-mode
    (setq flymake-grammarly--done-checking t)
    (flymake-mode 1)))

(add-to-list 'grammarly-on-open-function-list 'flymake-grammarly--on-open)
(add-to-list 'grammarly-on-message-function-list 'flymake-grammarly--on-message)
(add-to-list 'grammarly-on-close-function-list 'flymake-grammarly--on-close)

;;; Core

(defun flymake-grammarly--minified-string (str)
  "Minify the STR to check if any text changed."
  (declare (side-effect-free t))
  (md5 (replace-regexp-in-string "[[:space:]\n]+" " " str)))

(defun flymake-grammarly--kill-timer ()
  "Kill the timer."
  (when (timerp flymake-grammarly--request-timer)
    (cancel-timer flymake-grammarly--request-timer)
    (setq flymake-grammarly--request-timer nil)))

(defun flymake-grammarly--reset-request ()
  "Reset some variables so the next time the user done typing can reuse."
  (flymake-grammarly--debug-message "[INFO] Reset grammarly requests!")
  (setq flymake-grammarly--last-buffer-string (buffer-string)
        flymake-grammarly--point-data '()
        flymake-grammarly--done-checking nil))

(defun flymake-grammarly--after-change-functions (&rest _)
  "After change function to check if content change."
  (unless (string=
           (flymake-grammarly--minified-string flymake-grammarly--last-buffer-string)
           (flymake-grammarly--minified-string (buffer-string)))
    (flymake-grammarly--kill-timer)
    (setq flymake-grammarly--request-timer
          (run-with-timer flymake-grammarly-check-time nil
                          'flymake-grammarly--reset-request))))

(defun flymake-grammarly--encode-char (char-code)
  "Turn CHAR-CODE to character string."
  (cl-case char-code
    (4194208 (cons " " 2))
    (4194201 (cons "'" 3))
    (t nil)))

(defun flymake-grammarly--html-to-text (html)
  "Turn HTML to text."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((replace-data (flymake-grammarly--encode-char (char-before))))
        (when replace-data
          (backward-delete-char (cdr replace-data))
          (insert (car replace-data))))
      (forward-char 1))
    (dom-texts (libxml-parse-html-region (point-min) (point-max)))))

(defun flymake-grammarly--grab-info (data attr)
  "Grab value through ATTR key with DATA."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-from-string data)))
    (gethash attr json)))

(defun flymake-grammarly--valid-description (desc)
  "Convert to valid description DESC."
  (replace-regexp-in-string "\n" "" desc))

(defun flymake-grammarly--check-all (source-buffer)
  "Check grammar for SOURCE-BUFFER document."
  (let ((check-list '()))
    (dolist (data flymake-grammarly--point-data)
      (let ((type (if (string-match-p "error" data) :error :warning))
            (pt-beg (flymake-grammarly--grab-info data "highlightBegin"))
            (pt-end (flymake-grammarly--grab-info data "highlightEnd"))
            (desc (flymake-grammarly--html-to-text
                   (flymake-grammarly--grab-info data "explanation"))))
        (setq desc (flymake-grammarly--valid-description desc))
        (push (flymake-make-diagnostic source-buffer (1+ pt-beg) (1+ pt-end) type desc) check-list)))
    check-list))

(defun flymake-grammarly--grammar-check ()
  "Grammar check once."
  (unless flymake-grammarly--done-checking
    (flymake-grammarly--reset-request)
    (grammarly-check-text (buffer-string))))

;;; Flymake

(defvar flymake-grammarly--report-fnc nil
  "Record report function/execution.")

(defvar flymake-grammarly--source-buffer nil
  "Record source check buffer.")

(defun flymake-grammarly--report-once ()
  "Report with flymake after done requesting."
  (when (functionp flymake-grammarly--report-fnc)
    (flymake-grammarly--grammar-check)
    (funcall flymake-grammarly--report-fnc
             (flymake-grammarly--check-all flymake-grammarly--source-buffer))))

(defun flymake-grammarly--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (setq flymake-grammarly--report-fnc report-fn
        flymake-grammarly--source-buffer (current-buffer))
  (flymake-grammarly--report-once))

;;; Entry

;;;###autoload
(defun flymake-grammarly-load ()
  "Configure flymake mode to check the current buffer's grammar."
  (interactive)
  (setq flymake-grammarly--last-buffer-string (buffer-string))
  (flymake-grammarly--grammar-check)
  (add-hook 'after-change-functions #'flymake-grammarly--after-change-functions nil t)
  (add-hook 'flymake-diagnostic-functions #'flymake-grammarly--checker nil t))

;;;###autoload
(defun flymake-grammarly-maybe-load ()
  "Call `flymake-grammarly-load' if this file appears to be check for grammar."
  (interactive)
  (when (memq major-mode flymake-grammarly-active-modes)
    (flymake-grammarly-load)))

(provide 'flymake-grammarly)
;;; flymake-grammarly.el ends here
