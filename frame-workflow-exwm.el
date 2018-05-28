;;; frame-workflow-exwm.el --- Integrate frame-workflow with EXWM -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((emacs "25.1") (exwm "0.18"))
;; Keywords: frames unix
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

;; This library provides utilities to integrate frame-workflow with EXWM.

;;; Code:

(require 'exwm-workspace)
(require 'cl-lib)

(defun frame-workflow-exwm--visible-workspace-index-alist ()
  "Get an alist of visible EXWM workspaces."
  (cl-loop for i from 0 to (1- (exwm-workspace--count))
           for frm = (exwm-workspace--workspace-from-frame-or-index i)
           when (exwm-workspace--active-p frm)
           collect (cons i frm)))

(defun frame-workflow-exwm--format-with-index (index frame)
  "Return a string of INDEX:FRAME."
  (let ((name (frame-parameter frame 'workflow-prototype)))
    (concat (int-to-string index)
            (if name
                (concat ":" (symbol-name name))
              ""))))

(defun frame-workflow-exwm--2-combinations (list)
  "Return all combinations from LIST."
  (cl-loop for (i . r) on list by 'cdr
           append (cl-loop for j in r
                           collect (list i j))))

(defun frame-workflow-exwm--select-two-visible-workspaces ()
  "Select two visible workspaces."
  (pcase (frame-workflow-exwm--visible-workspace-index-alist)
    ;; Only one workspace: Do nothing
    (`(,_) nil)
    ;; Two workspaces
    (`((,_ . ,w1) (,_ . ,w2)) (list w1 w2))
    ;; More than two workspaces: Select two via completing-read
    (wss (let* ((selections (frame-workflow-exwm--2-combinations wss))
                (candidates (cl-loop for ((i1 . w1) (i2 . w2)) in selections
                                     collect (cons (concat (frame-workflow-exwm--format-with-index i1 w1)
                                                           " <-> "
                                                           (frame-workflow-exwm--format-with-index i2 w2))
                                                   (cons w1 w2))))
                (result (completing-read "Workspaces to swap: " candidates nil t)))
           (pcase (cdr (assoc result candidates))
             (`(,w1 . ,w2) (list w1 w2)))))))

;;;###autoload
(defun frame-workflow-exwm-swap-workspaces (w1 w2)
  "Swap two workspaces.

W1 and W2 are frames.

If this function is called interactively, the user can select two workspaces
from visible workspaces."
  (interactive (frame-workflow-exwm--select-two-visible-workspaces))
  (exwm-workspace-swap w1 w2))

(provide 'frame-workflow-exwm)
;;; frame-workflow-exwm.el ends here
