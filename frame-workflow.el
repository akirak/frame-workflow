;;; frame-workflow.el --- Frame-oriented workflow -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.9
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

;; This is a rewrite of frame-workflow using EIEIO.

;;; Code:

(require 'eieio)
(require 'eieio-base)
(require 'subr-x)

;;;; Variables

(defvar frame-workflow--subject-list nil "Used by `eieio-instance-tracker'.")
(defvar frame-workflow--observer-list nil "Used by `eieio-instance-tracker'.")

;;;; Subjects

(defclass frame-workflow-subject (eieio-instance-tracker
                                  eieio-instance-inheritor
                                  ;; eieio-named
                                  ;; eieio-persistent
                                  )
  ((tracking-symbol :initform frame-workflow--subject-list)
   (name :initarg :name
         :type string
         :documentation "Name to uniquely identify this type.")
   (observer-class :initarg :observer-class
                   :initform 'frame-workflow-observer
                   :type class
                   :documentation "`frame-workflow-observer' class or its subclass.")
   ;; TODO
   ;; (layout :initarg :layout
   ;;         :documentation "S-expression evalated to set up windows in each frame.")
   (make-frame :initarg :make-frame
               :type function
               :initform #'make-frame
               :documentation "A function that returns a new frame."))
  "An object that specifies workflow on a frame.")

(defmacro frame-workflow-define-subject (name &rest args)
  "Define a workflow subject.

NAME is a string to uniquely identify the subject.

ARGS is a plist of arguments passed to `frame-workflow-subject'."
  ;; TODO: Prevent creating duplicate objects of the same name
  (declare (indent 1))
  `(make-instance 'frame-workflow-subject :name ,name ,@args))

(cl-defmethod frame-workflow--make-frame ((subject frame-workflow-subject))
  "Create a frame of SUBJECT.

SUBJECT is an object of `frame-workflow-subject' class or its subclass."
  (let* ((observer-class (oref subject observer-class))
         (frame (funcall (oref subject make-frame)))
         (observer (make-instance (if (fboundp observer-class)
                                      observer-class
                                    'frame-workflow-observer)
                                  :type subject :frame frame)))
    (set-frame-parameter frame 'workflow observer)
    frame))

;;;; Observers

(defclass frame-workflow-observer (eieio-instance-tracker)
  ((tracking-symbol :initform frame-workflow--observer-list)
   (subject :initarg :type
            :type frame-workflow-subject
            :documentation "Frame subject which created this frame.")
   (subject-name :documentation "Name of the frame type."
                 :reader frame-workflow--subject-name)
   (frame :initarg :frame
          :type frame
          :documentation "Emacs frame object."))
  "An object that tracks events on a frame and holds ephemeral states.")

(cl-defmethod frame-workflow--subject-name ((obj frame-workflow-observer))
  (oref (oref obj subject) name))

;;;; Utility functions

(defun frame-workflow--subject-names ()
  "Return a list of names lf all subject objects."
  (mapcar (lambda (obj) (oref obj :name)) frame-workflow--subject-list))

(defun frame-workflow--find-subject (name)
  "Find a subject object with NAME."
  (eieio-instance-tracker-find name 'name 'frame-workflow--subject-list))

(defun frame-workflow--other-frame-subject-names ()
  "Types names of frames excluding the selected frame."
  (cl-loop for instance in frame-workflow--observer-list
           unless (equal (oref instance frame) (selected-frame))
           collect (frame-workflow--subject-name instance)))

;;;; Interactive commands

(defun frame-workflow-make-frame (subject)
  "Create a frame of workflow SUBJECT."
  (interactive (list (completing-read "Create a frame: "
                                      (frame-workflow--subject-names))))
  (if-let ((subject (cl-etypecase subject
                      (string (frame-workflow--find-subject subject))
                      (frame-workflow-subject subject))))
      (frame-workflow--make-frame subject)
    (error "There is no subject named %s" subject)))

(defun frame-workflow-select-frame (subject)
  "Select a frame of workflow SUBJECT."
  (interactive (list (completing-read "Select a frame: "
                                      (frame-workflow--other-frame-subject-names))))
  (when-let ((frame (eieio-instance-tracker-find subject
                                                 'subject-name
                                                 'frame-workflow--observer-list)))
    (select-frame frame)
    frame))

(defun frame-workflow-switch-frame (subject)
  "Select or create a frame of workflow SUBJECT."
  (interactive (list (completing-read "Select or create a frame: "
                                      (frame-workflow--subject-names))))
  (or (frame-workflow-select-frame subject)
      (frame-workflow-make-frame subject)))

(defun frame-workflow-identify ()
  "Display information on the workflow of the selected frame."
  (interactive)
  (if-let ((workflow (frame-parameter nil 'workflow)))
      (message (frame-workflow--subject-name workflow))
    (message "No workflow")))

(provide 'frame-workflow)
;;; frame-workflow.el ends here
