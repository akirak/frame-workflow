;;; frame-workflow-editor.el --- Temporary buffer for editing a Lisp object -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/akirak/frame-workflow

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides `frame-workflow-editor-popup' command which creates a
;; temporary buffer of `emacs-lisp-mode'.

;;; Code:

(defvar frame-workflow-editor-on-done)
(make-variable-buffer-local 'frame-workflow-editor-on-done)

(defvar frame-workflow-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'frame-workflow-editor-done)
    (define-key map (kbd "C-c C-k") #'frame-workflow-editor-cancel)
    map)
  "Keymap for `frame-workflow-editor-mode'.")

(define-derived-mode frame-workflow-editor-mode emacs-lisp-mode
  "Major mode for editing a Lisp object or expression."
  (use-local-map frame-workflow-editor-mode-map))

(defun frame-workflow-editor-popup (bufname insert-content on-done)
  "Pop to a buffer for editing a Lisp expression.

BUFNAME is the name of a buffer. If there is a buffer of the same name, confirm
discarding it."
  (declare (indent 1))
  (catch 'abort
    (let (buf)
      (setq buf (get-buffer bufname))
      (when (and buf (buffer-modified-p buf))
        (if (yes-or-no-p (format "Buffer %s already exists and modified. Discard it?"
                                 bufname))
            (progn (kill-buffer buf)
                   (setq buf nil))
          (pop-to-buffer buf)
          (throw 'abort)))
      (with-current-buffer (or buf (generate-new-buffer bufname))
        (erase-buffer)
        (kill-all-local-variables)
        (funcall insert-content)
        (set-buffer-modified-p nil)
        (frame-workflow-editor-mode)
        (setq frame-workflow-editor-on-done on-done)
        (setq-local header-line-format "  C-c C-c to save, C-c C-k to cancel")
        (pop-to-buffer (current-buffer))))))

(defun frame-workflow-editor-done ()
  (interactive)
  (funcall frame-workflow-editor-on-done)
  (kill-buffer))

(defun frame-workflow-editor-cancel ()
  (interactive)
  (message "Aborted")
  (kill-buffer))

(provide 'frame-workflow-editor)
;;; frame-workflow-editor.el ends here
