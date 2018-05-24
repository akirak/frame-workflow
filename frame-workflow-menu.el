;;; frame-workflow-menu.el --- Menu bar entries for frame-workflow -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: frames
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

;; This library provides menu bar entries related to frame-workflow.

;;; Code:

(require 'frame-workflow)
(require 'easymenu)

(defun frame-workflow-menu--frame-entry (frame)
  "Create a submenu entry for FRAME."
  (vector (frame-parameter frame 'name)
          `(lambda () (interactive) (select-frame--select-frame ,frame)))
  ;; I considered adding per-frame window lists, but I found it redundant.
  ;; (cons (frame-parameter frame 'name)
  ;;       (mapcar (lambda (wnd)
  ;;                 (vector (buffer-name (window-buffer wnd))
  ;;                         `(lambda () (interactive) (select-window ,wnd))))
  ;;               (window-list frame nil)))
  )

(defun frame-workflow-menu--current-frame-entry ()
  "Create a submenu entry for the current frame."
  (let ((prototype (frame-workflow--prototype-name)))
    `(,(format "<<%s>>" (symbol-name prototype))
      ,(frame-parameter nil 'name)
      "---"
      ,@(cl-loop for (label . command) in (frame-workflow--action-alist-by-name)
                 collect (vector label (frame-workflow--action-to-command command)))
      "---"
      ["Add a new action"
       frame-workflow-add-new-action]
      ["Save the actions to the prototype"
       frame-workflow-save-actions-to-prototype]
      "---"
      ,(cond
        (prototype (vector (format "Reapply %s" (symbol-name prototype))
                           'frame-workflow-apply-prototype))
        ((frame-workflow--has-workflow) "Cannot apply a prototype")
        (t (list "Apply a prototype" :filter
                 (lambda (&rest _args)
                   (mapcar
                    (lambda (name)
                      (vector (symbol-name name)
                              `(lambda () (interactive)
                                 (frame-workflow-apply-prototype (quote ,name)))))
                    (frame-workflow-prototype--configured-names)))))))))

(defun frame-workflow-menu--other-frame-entries ()
  "Create menu entries for other frames grouped by prototype names."
  (mapcar (lambda (grp)
            (cons (symbol-name (car grp))
                  (mapcar #'frame-workflow-menu--frame-entry
                          (cdr grp))))
          (seq-group-by #'frame-workflow--prototype-name
                        (cl-remove (selected-frame) (frame-list)))))

(defun frame-workflow-menu--make-new-frame-entry ()
  "Create a submenu entry to create a new frame from a prototype."
  '("Make a new frame" :filter
    (lambda (&rest _args)
      (mapcar (lambda (prototype)
                (vector (symbol-name prototype)
                        `(lambda () (interactive)
                           (frame-workflow-make-frame (quote ,prototype)))))
              (frame-workflow-prototype--configured-names)))))

(easy-menu-define frame-workflow-menu global-map
  "Menu for `frame-workflow'."
  '("FrameWorkflow" :filter
    (lambda (&rest _args)
      `(,(frame-workflow-menu--current-frame-entry)
        "---"
        ,@(frame-workflow-menu--other-frame-entries)
        "---"
        ,(frame-workflow-menu--make-new-frame-entry)
        ("Customize"
         ["Edit prototypes" (customize-variable 'frame-workflow-prototype-alist)]
         ["Save prototypes to custom-file" frame-workflow-prototype-save-all]
         ["Load prototypes from custom-file" frame-workflow-prototype-reload-all]
         ["Customize frame-workflow group" (customize-group 'frame-workflow)])))))

(provide 'frame-workflow-menu)
;;; frame-workflow-menu.el ends here
