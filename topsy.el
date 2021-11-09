;;; topsy.el --- Simple sticky header  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/topsy.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.3"))
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

;; This library shows a sticky header at the top of the window.  The
;; header shows which definition the top line of the window is within.
;; Intended as a simple alternative to `semantic-stickyfunc-mode`.

;; Mode-specific functions may be added to `topsy-mode-functions'.

;; NOTE: For Org mode buffers, please use org-sticky-header:
;; <https://github.com/alphapapa/org-sticky-header>.

;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'face-remap)

;;;; Variables

(defconst topsy-header-line-format
  '(:eval (list (propertize " " 'display '((space :align-to 0)))
                (topsy--header-string)))
  "The header line format used by `topsy-mode'.")
(put 'topsy-header-line-format 'risky-local-variable t)

(defvar-local topsy-old-hlf nil
  "Preserves the old value of `header-line-format'.")

(defvar-local topsy-fn nil
  "Function that returns the header in a buffer.")

(defvar-local topsy--face-remap nil
  "Cookie returned by `face-remap-add-relative'.")

;;;; Customization

(defgroup topsy nil
  "Show a sticky header at the top of the window.
The header shows which definition the top line of the window is
within.  Intended as a simple alternative to
`semantic-stickyfunc-mode`."
  :group 'convenience)

(defcustom topsy-mode-functions
  '((emacs-lisp-mode . topsy--beginning-of-defun)
    (magit-section-mode . topsy--magit-section)
    (org-mode . (lambda ()
                  "topsy: Please use package `org-sticky-header' for Org mode"))
    (nil . topsy--beginning-of-defun))
  "Alist mapping major modes to functions.
Each function provides the sticky header string in a mode.  The
nil key defines the default function."
  :type '(alist :key-type symbol
                :value-type function))

(defface topsy '((t :inherit default))
  "Face with which to display sticky header.
To match the appearance of the text in the buffer, the `default`
face should be included in the :inherit specification.
Otherwise, unspecified face attributes will be inherited from the
`header-line` face.

To use the header-line, remove the :inherit attribute.")

(defface topsy-highlight '((t :weight bold :underline t))
  "Face for sticky header when it is showing the first line of a defun.
This face will be used only when a defun is partially visible and
the sticky header is showing its first line.  The faces of the
buffer text being shown have higher priority than this face.")

(defcustom topsy-previous-line-fallback t
  "If no sticky header string is found, show the line above the window start."
  :type '(choice (const :tag "Show previous line" t)
                 (const :tag "Leave header blank" nil)))

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
        (setf topsy-fn (or (alist-get major-mode topsy-mode-functions)
                           (alist-get nil topsy-mode-functions))
              header-line-format 'topsy-header-line-format)
        (setq topsy--face-remap
              (face-remap-add-relative 'header-line 'topsy)))
    ;; Disable mode
    (when (eq header-line-format 'topsy-header-line-format)
      ;; Restore previous buffer local value of header line format if
      ;; the current one is the sticky func one.
      (kill-local-variable 'header-line-format)
      (when topsy-old-hlf
        (setf header-line-format topsy-old-hlf
              topsy-old-hlf nil)))
    (face-remap-remove-relative topsy--face-remap)))

;;;; Functions

(defun topsy--header-string ()
  "Return string found by `topsy-fn' or line above window start.
When `topsy-fn' returns a string, it is propertized with the
'topsy-highlight face.  When its return value is nil, return the
string above the top of the window."
  (or (when-let ((header (and topsy-fn (funcall topsy-fn))))
        (add-face-text-property 0 (length header) 'topsy-highlight t header)
        header)
      (when topsy-previous-line-fallback
        (save-excursion
          (goto-char (window-start))
          (vertical-motion -1)
          (let ((bol (point))
                (eol (1- (window-start))))
            (font-lock-ensure bol eol)
            (buffer-substring bol eol))))))

(defun topsy--beginning-of-defun ()
  "Return the first line of a partially visible defun.
The beginning and end of the defun are identified by
`beginning-of-defun' and `end-of-defun', respectively, with
buffer narrowing is ignored

Return nil if no defun is partially visible."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (window-start))
      (let ((bod (ignore-errors (beginning-of-defun) (point)))
            (eol (point-at-eol))
            (eod (ignore-errors (end-of-defun) (point))))
        (when (and bod (< bod (window-start))
                   (or (not eod) (>= eod (window-start))))
          (font-lock-ensure bod eol)
          (buffer-substring bod eol))))))

(defun topsy--magit-section ()
  "Return the header line in a `magit-section-mode' buffer."
  (cl-labels ((level-of
               (section) (length (magit-section-ident section)))
              (parent-of
               (section) (save-excursion
                           (goto-char (oref section start))
                           (let ((old-level (level-of section))
                                 (old-pos (point)))
                             (magit-section-up)
                             (when (and (/= old-level (level-of (magit-current-section)))
                                        (/= old-pos (point)))
                               (magit-current-section))))))
    (save-excursion
      (goto-char (window-start))
      (when-let (strings
		 (cl-loop with current-section = (magit-current-section)
                          when (and (oref current-section content)
                                    (/= (window-start) (oref current-section start)))
                          collect (string-trim
				   (buffer-substring
				    (oref current-section start)
				    (oref current-section content)))
                          for parent-section = (parent-of current-section)
                          while parent-section
                          do (setf current-section parent-section)))
	(string-join strings " « ")))))

;;;; Footer

(provide 'topsy)

;;; topsy.el ends here
