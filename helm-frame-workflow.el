;;; helm-frame-workflow.el --- Helm sources for frame-workflow -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0

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

;;; Code:

(require 'frame-workflow)
(require 'helm)
(require 'cl-lib)

;;;; Actions
(defvar helm-frame-workflow-frame-actions
  '(("Select the frame" . frame-workflow--select-frame))
  "Alist of actions on a frame.")

(defvar helm-frame-workflow-subject-actions
  '(("Make a frame" . frame-workflow-make-frame))
  "Alist of actions on a subject.")

;;;; Classes for sources

(defclass helm-frame-workflow-frames-class (helm-source-sync)
  ((candidates :initform
               (lambda ()
                 (mapcar (lambda (observer)
                           (let* ((frame (oref observer frame))
                                  (subject (frame-workflow--subject-name observer))
                                  (title (frame-parameter frame 'name)))
                             ;; TODO: Customizable format
                             (cons (format "%-10s : %s" subject title)
                                   frame)))
                         frame-workflow--observer-list)))
   (action :initform 'helm-frame-workflow-frame-actions)))

(defclass helm-frame-workflow-no-subject-frames-class (helm-source-sync)
  ((candidates :initform
               (lambda ()
                 (cl-loop for frame in (frame-list)
                          unless (frame-parameter frame 'workflow)
                          collect (cons (frame-parameter frame 'name)
                                        frame))))
   (action :initform 'helm-frame-workflow-frame-actions)))

(defclass helm-frame-workflow-subjects-class (helm-source-sync)
  ((candidates :initform
               (lambda ()
                 (mapcar (lambda (subject)
                           (cons (oref subject name)
                                 subject))
                         frame-workflow--subject-list)))
   (action :initform 'helm-frame-workflow-subject-actions)))

;;;; Concrete sources

(defvar helm-frame-workflow-source-frames
  (helm-make-source "Frames with workflow"
      'helm-frame-workflow-frames-class)
  "Helm source for frames with a subject.")

(defvar helm-frame-workflow-source-no-subject-frames
  (helm-make-source "Frames without workflow"
      'helm-frame-workflow-no-subject-frames-class)
  "Helm source for frames without a subject.")

(defvar helm-frame-workflow-source-subjects
  (helm-make-source "Workflow subjects"
      'helm-frame-workflow-subjects-class)
  "Helm source for workflow subjects.")

;;;; Command

;;;###autoload
(defun helm-frame-workflow ()
  "Helm command for frame-workflow."
  (interactive)
  (helm :sources '(helm-frame-workflow-source-frames
                   helm-frame-workflow-source-no-subject-frames
                   helm-frame-workflow-source-subjects)
        :buffer "*helm frame-workflow*"
        :prompt "frame-workflow: "))

(provide 'helm-frame-workflow)
;;; helm-frame-workflow.el ends here
