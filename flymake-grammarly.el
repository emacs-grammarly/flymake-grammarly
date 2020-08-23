;;; flymake-grammarly.el --- Grammarly support for Flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-08-23 17:21:13

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Grammarly support for Flymake.
;; Keyword: grammar check
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (grammarly "0.1.0") (flymake-easy "0.9")))
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
;; Grammarly support for Flymake.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'dom)

(require 'flymake-easy)
(require 'grammarly)

(defgroup flymake-grammarly nil
  "Grammarly support for Flymake."
  :prefix "flymake-grammarly-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/jcs-elpa/flymake-grammarly"))

(defconst flymake-grammarly-err-line-patterns
  '(("^\\(.+\\)\: line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$" nil 2 3 4))
  "Error line pattern definition.")

(defcustom flymake-grammarly-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of mode will enable grammarly in the buffer."
  :type 'list
  :group 'flymake-grammarly)

;;;###autoload
(defun flymake-grammarly-load ()
  "Configure flymake mode to check the current buffer's grammar."
  (interactive)
  (flymake-easy-load 'flymake-grammarly-command
                     flymake-grammarly-err-line-patterns
                     'tempdir
                     "grammarly"))

;;;###autoload
(defun flymake-grammarly-maybe-load ()
  "Call `flymake-grammarly-load' if this file appears to be check for grammar."
  (interactive)
  (when (memq major-mode flymake-grammarly-active-modes)
    (flymake-grammarly-load)))

(provide 'flymake-grammarly)
;;; flymake-grammarly.el ends here
