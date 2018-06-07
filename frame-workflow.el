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

;;;;; Modeline

(defcustom frame-workflow-mode-line
  '(:eval (concat " Frame"
                  ;; TODO: Make this a separate function
                  (if-let ((name (frame-workflow--frame-subject-name)))
                      (format "[%s]" name)
                    "")))
  "Mode line lighter for frame-workflow."
  :group 'frame-workflow
  :type 'sexp
  :risky t)

;;;; Minor mode

(define-minor-mode frame-workflow-mode
  "Toggle `frame-workflow-mode`."
  :global t
  :group 'frame-workflow
  :require 'frame-workflow
  :lighter frame-workflow-mode-line
  :init-value nil
  (if frame-workflow-mode
      (frame-workflow--enable)
    (frame-workflow--disable)))

(defun frame-workflow--enable ()
  "Turn on `frame-workflow-mode'."
  (add-hook 'delete-frame-functions 'frame-workflow--delete-frame t))

(defun frame-workflow--disable ()
  "Turn off `frame-workflow-mode'."
  (remove-hook 'delete-frame-functions 'frame-workflow--delete-frame))

(defun frame-workflow--delete-frame (frame)
  "Run hooks for frame-workflow on deleting FRAME."
  (when-let ((observer (frame-parameter frame 'workflow)))
    (delete-instance observer)))

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
               :type (or function list)
               :initform #'make-frame
               :documentation "A function that returns a new frame."))
  "An object that specifies workflow on a frame.")

(defmacro frame-workflow-define-subject (name &rest args)
  "Define a workflow subject.

NAME is a string to uniquely identify the subject.

ARGS is a plist of arguments passed to `frame-workflow-subject'."
  (declare (indent 1))
  (let ((existing (frame-workflow--find-subject name))
        (new (apply #'make-instance 'frame-workflow-subject :name name args)))
    (when existing
      (frame-workflow--replace-subject existing new))
    new))

(defun frame-workflow--replace-subject (old new)
  "Replace a subject OLD with NEW.

This is used to update a subject of the same name."
  ;; Replace references to old subjects from observers
  (cl-loop for observer in frame-workflow--observer-list
           when (eq (oref observer subject) old)
           do (oset observer subject new))
  ;; Delete the old subject from the instance list
  (delete-instance old))

(cl-defmethod frame-workflow--make-frame ((subject frame-workflow-subject))
  "Create a frame of SUBJECT.

SUBJECT is an object of `frame-workflow-subject' class or its subclass."
  (let* ((observer-class (oref subject observer-class))
         (local-make-frame (oref subject make-frame))
         (frame (cl-etypecase local-make-frame
                  (function (funcall local-make-frame))
                  (list (eval local-make-frame))))
         (observer (make-instance (if (fboundp observer-class)
                                      observer-class
                                    'frame-workflow-observer)
                                  :subject subject :frame frame)))
    (set-frame-parameter frame 'workflow observer)
    frame))

;;;; Observers

(defclass frame-workflow-observer (eieio-instance-tracker)
  ((tracking-symbol :initform frame-workflow--observer-list)
   (subject :initarg :subject
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
  (mapcar (lambda (obj) (oref obj name)) frame-workflow--subject-list))

(defun frame-workflow--find-subject (name)
  "Find a subject object with NAME."
  (eieio-instance-tracker-find name 'name 'frame-workflow--subject-list))

(defun frame-workflow--other-frame-subject-names ()
  "Types names of frames excluding the selected frame."
  (cl-loop for instance in frame-workflow--observer-list
           unless (equal (oref instance frame) (selected-frame))
           collect (frame-workflow--subject-name instance)))

(defun frame-workflow--frame-observer (&optional frame)
  "Return the observer of FRAME if it has one."
  (let ((obj (frame-parameter frame 'workflow)))
    (when (frame-workflow-observer-p obj)
      obj)))

(defun frame-workflow--frame-subject-name (&optional frame)
  "Get the subject name of FRAME."
  (when-let ((observer (frame-workflow--frame-observer frame)))
    (frame-workflow--subject-name observer)))

(defun frame-workflow--find-frame-by-subject (name)
  "Find a frame by the NAME of a subject.

If there are multiple frames of the subject, this returns only the first one."
  (when-let ((subject (frame-workflow--find-subject name))
             (observer (eieio-instance-tracker-find subject 'subject
                                                    'frame-workflow--observer-list)))
    (oref observer frame)))

(defun frame-workflow--select-frame (frame)
  "Internal function to select FRAME."
  ;; TODO: Make this customizable
  (select-frame-set-input-focus frame))

;;;; Interactive commands

(defun frame-workflow-make-frame (subject)
  "Create a frame of workflow SUBJECT."
  (interactive (list (completing-read "Create a frame: "
                                      (frame-workflow--subject-names))))
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (if-let ((subject (cl-etypecase subject
                      (string (frame-workflow--find-subject subject))
                      (frame-workflow-subject subject))))
      (frame-workflow--make-frame subject)
    (user-error "There is no subject named %s" subject)))

(defun frame-workflow-select-frame (subject)
  "Select a frame of workflow SUBJECT."
  (interactive (list (completing-read "Select a frame: "
                                      (frame-workflow--other-frame-subject-names))))
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (when-let ((frame (frame-workflow--find-frame-by-subject subject)))
    (frame-workflow--select-frame frame)
    frame))

(defun frame-workflow-switch-frame (subject)
  "Select or create a frame of workflow SUBJECT."
  (interactive (list (completing-read "Select or create a frame: "
                                      (frame-workflow--subject-names))))
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (or (frame-workflow-select-frame subject)
      (frame-workflow-make-frame subject)))

(defun frame-workflow-identify ()
  "Display information on the workflow of the selected frame."
  (interactive)
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (if-let ((workflow (frame-parameter nil 'workflow)))
      (message (frame-workflow--subject-name workflow))
    (message "No workflow")))

(provide 'frame-workflow)
;;; frame-workflow.el ends here
