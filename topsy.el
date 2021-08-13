;;; topsy.el --- Simple sticky header lines for many modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;;;; License:

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

;; Display a sticky header at the top of a window that shows useful
;; information, like which top-level Lisp form extends beyond the top
;; of the window.  Intended as a simple alternative to
;; `semantic-stickyfunc-mode`.

;;; Code:

;;;; Requirements


;;;; Variables

(defconst topsy-header-line-format
  '(:eval (list (propertize " " 'display '((space :align-to 0)))
                (funcall topsy-fn)))
  "The header line format used by `topsy-mode'.")
(put 'topsy-header-line-format 'risky-local-variable t)

(defvar-local topsy-old-hlf nil
  "Preserves the old value of `header-line-format'.")

(defvar-local topsy-fn nil
  "Function that returns the header in a buffer.")

;;;; Customization

(defgroup topsy nil
  "Options for `topsy'."
  :group 'convenience)

(defcustom topsy-major-mode-map
  '((emacs-lisp-mode . topsy--lisp))
  "Alist mapping major modes to functions.
Each function provides the sticky header string in a mode."
  :type '(alist :key-type symbol
                :value-type function))

;;;; Commands

;;;###autoload
(define-minor-mode topsy-mode
  "Minor mode to show a simple sticky header.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the minor mode is enabled."
  :group 'topsy
  (if topsy-mode
      (progn
        (when (and (local-variable-p 'header-line-format (current-buffer))
                   (not (eq header-line-format topsy-header-line-format)))
          ;; Save previous buffer local value of header line format.
          (setf topsy-old-hlf header-line-format))
        ;; Enable the mode
        (setf topsy-fn (alist-get major-mode topsy-major-mode-map)
              header-line-format 'topsy-header-line-format))
    ;; Disable mode
    (when (eq header-line-format 'topsy-header-line-format)
      ;; Restore previous buffer local value of header line format if
      ;; the current one is the sticky func one.
      (kill-local-variable 'header-line-format)
      (when topsy-old-hlf
        (setf header-line-format topsy-old-hlf
              topsy-old-hlf nil)))))

;;;; Functions

(defun topsy--lisp ()
  "Return the first line of the top level form that extends beyond the window start."
  (when (> (window-start) 1)
    (save-excursion
      (goto-char (window-start))
      (beginning-of-defun)
      (font-lock-ensure (point) (point-at-eol))
      (buffer-substring (point) (point-at-eol)))))

;;;; Footer

(provide 'topsy)

;;; topsy.el ends here
