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
(autoload 'eieio-customize-object "eieio-custom")
(require 'frame-workflow-editor)

(declare-function #'magit-status "magit")
(declare-function #'magit-display-buffer-same-window-except-diff-v1 "magit")
(declare-function #'frame-purpose-make-directory-frame "frame-purpose")

;;;; Variables

(defvar frame-workflow--subject-list nil "Used by `eieio-instance-tracker'.")
(defvar frame-workflow--observer-list nil "Used by `eieio-instance-tracker'.")

(defvar frame-workflow--buffer-killed nil
  "Observer instance where a buffer was killed.")

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

;;;; Other customizations

(defcustom frame-workflow-directory-frame-action
  (if (fboundp #'magit-status)
      #'frame-workflow-magit-same-window
    (lambda () (dired default-directory)))
  "Function called after a directory frame is created.

This function is called in `frame-workflow-make-directory-frame'
after a directory frame is called.  The function is called with
`default-directory' set to the directory.

If this value is nil, no function is called in directory frames."
  :type 'function
  :group 'frame-workflow)

(defcustom frame-workflow-use-frame-purpose-for-directory t
  "Use frame-purpose to create a directory frame.

If this value is non-nil, directory subjects defined by
`frame-workflow-make-directory-frame' uses
`frame-purpose-make-directory-frame' to create a new frame for a given
directory.  The frame becomes a purpose-specific frame for
the directory, which makes it easy for you to focus on files in the
directory."
  :type 'boolean
  :group 'frame-workflow)

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
  (add-hook 'kill-buffer-hook 'frame-workflow--kill-buffer-hook t)
  (add-hook 'delete-frame-functions 'frame-workflow--delete-frame t))

(defun frame-workflow--disable ()
  "Turn off `frame-workflow-mode'."
  (remove-hook 'kill-buffer-hook 'frame-workflow--kill-buffer-hook)
  (remove-hook 'delete-frame-functions 'frame-workflow--delete-frame))

(defun frame-workflow--delete-frame (frame)
  "Run hooks for frame-workflow on deleting FRAME."
  (when-let ((observer (frame-parameter frame 'workflow)))
    (delete-instance observer)))

(defun frame-workflow--kill-buffer-hook ()
  "Hook run when a buffer is being killed."
  (when-let ((window (get-buffer-window)))
    (setq frame-workflow--buffer-killed
          (frame-workflow--frame-observer (window-frame window)))
    (run-with-timer 0.025 nil #'frame-workflow--post-kill-buffer)))

(defun frame-workflow--post-kill-buffer ()
  "Hook run after a buffer is killed."
  (unwind-protect
      ;; TODO: Is it possible to explicitly cast an EIEIO object?
      (when-let ((observer frame-workflow--buffer-killed)
                 (frame (oref observer frame))
                 (subject (oref observer subject))
                 (after-kill-buffer (oref subject after-kill-buffer)))
        (with-selected-frame frame
          (eval after-kill-buffer)))
    (setq frame-workflow--buffer-killed nil)))

;;;; Subjects

(defclass frame-workflow-subject (eieio-instance-tracker
                                  eieio-instance-inheritor
                                  ;; eieio-named
                                  eieio-persistent)
  ((tracking-symbol :initform frame-workflow--subject-list)
   (name :initarg :name
         :type string
         :documentation "Name to uniquely identify this type.")
   (observer-class :initarg :observer-class
                   :initform 'frame-workflow-observer
                   :type class
                   :documentation "`frame-workflow-observer' class or its subclass.")
   (make-frame :initarg :make-frame
               :type (or function list)
               :initform #'make-frame
               :documentation "A function that returns a new frame.")
   (layout :initarg :layout
           :type list
           :initform nil
           :documentation "Lisp code run after frame creation.")
   (refocus :initarg :refocus
            :type list
            :initform nil
            :documentation "Lisp code run when explicitly switching to an
existing frame of the subject.")
   (after-kill-buffer :initarg :after-kill-buffer
                      :type list
                      :initform nil
                      :documentation "Lisp code run after a buffer is killed."))
  "An object that specifies workflow on a frame.")

(defun frame-workflow-define-subject (name &rest args)
  "Define a workflow subject.

NAME is a string to uniquely identify the subject.

ARGS is a plist of arguments passed to `frame-workflow-subject'."
  (declare (indent 1))
  (let ((existing (frame-workflow--find-subject name))
        (new (apply #'make-instance 'frame-workflow-subject
                    :name name :file name
                    args)))
    (when existing
      (frame-workflow--replace-subject existing new))
    new))

(cl-defmethod frame-workflow--replace-subject ((old frame-workflow-subject)
                                               (new frame-workflow-subject))
  "Replace a subject OLD with NEW.

This is used to update a subject of the same name."
  ;; Replace references to old subjects from observers
  (cl-loop for observer in frame-workflow--observer-list
           ;; FIXME: Fix warning \"Unknown slot ‘subject’\"
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
    (when-let ((layout (oref subject layout)))
      (with-selected-frame frame
        (eval layout)))
    frame))

;;;;; Interactive editing
(defun frame-workflow--subject-editor-name (name)
  "The name of a buffer to edit a subject with NAME."
  (format "*frame-workflow subject %s*" name))

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
             (observer (frame-workflow--find-observer 'subject subject)))
    (oref observer frame)))

(cl-defun frame-workflow--select-frame (frame)
  "Internal function to select FRAME."
  ;; TODO: Make this customizable
  (select-frame-set-input-focus frame))

(defun frame-workflow--find-observer (slot key &optional no-clean-up)
  "Find an observer instance that matches the condition.

This function internally uses `eieio-instance-tracker-find' to find an instance
whose SLOT is KEY.  It also deletes observers that are linked to dead frames
using `frame-workflow--clean-up-observers' before advance to ensure that
the result is \"live\". To skip this clean-up step, set NO-CLEAN-UP to non-nil."
  (unless no-clean-up
    (frame-workflow--clean-up-observers))
  (eieio-instance-tracker-find key slot 'frame-workflow--observer-list))

(defun frame-workflow--clean-up-observers ()
  "Delete observers that are linked to dead frames."
  (mapc (lambda (observer)
          (unless (frame-live-p (oref observer frame))
            (delete-instance observer)))
        frame-workflow--observer-list))

;;;; Interactive commands

(defun frame-workflow-make-frame (subject)
  "Create a frame of workflow SUBJECT."
  (interactive (list (completing-read "Create a frame: "
                                      (frame-workflow--subject-names))))
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (if-let ((subject (cl-etypecase subject
                      (string (or (frame-workflow--find-subject subject)
                                  (frame-workflow-define-subject subject)))
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
    (if (equal frame (selected-frame))
        (message "Same frame for %s" subject)
      (frame-workflow--select-frame frame))
    (when-let ((observer (frame-workflow--frame-observer frame))
               (subject (oref observer subject))
               (refocus-hook (oref subject refocus)))
      (eval refocus-hook))
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

(defun frame-workflow-edit-subject (subject)
  "Edit the definition of SUBJECT interactively."
  (interactive (list (completing-read "Edit a subject: "
                                      (frame-workflow--subject-names))))
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (if-let ((subject (cl-etypecase subject
                      (frame-workflow-subject subject)
                      (string (frame-workflow--find-subject subject))))
           (name (oref subject name)))
      (frame-workflow-editor-popup (frame-workflow--subject-editor-name name)
        (lambda ()
          (let ((standard-output (current-buffer)))
            (object-write subject)))
        (lambda ()
          (let ((tmpfile (make-temp-file name)))
            (write-file tmpfile)
            (let ((obj (eieio-persistent-read tmpfile 'frame-workflow-subject t)))
              (frame-workflow--replace-subject subject obj)))))
    (user-error "There is no subject named %s" subject)))

(defun frame-workflow-reload-layout (&optional frame)
  "Reload the default layout of FRAME."
  (interactive)
  (unless frame-workflow-mode
    (user-error "Please turn on `frame-workflow-mode'"))
  (when-let ((frame (or frame (selected-frame)))
             (observer (frame-workflow--frame-observer frame))
             (subject (oref observer subject))
             (layout (oref subject layout)))
    (with-selected-frame frame
      (eval layout))))

;;;; Extra features
;;;;; Directory subjects
(defun frame-workflow-make-directory-frame (&optional dir)
  "Create a new frame for DIR.

This is intended for use as `projectile-switch-project-action'.

This function defines a new subject for the given directory
(if it is not defined yet) and creates a frame for the subject.
The name of the created subject is the name of the directory without
its preceding path.

If `frame-purpose-mode' is turned on and
`frame-workflow-use-frame-purpose-for-directory' is non-nil,
the created frame becomes a directory-purpose frame.

If DIR is omitted, it defaults to `default-directory."
  (interactive)
  (let* ((dir (expand-file-name (or dir default-directory)))
         (name (file-name-nondirectory (string-remove-suffix "/" dir)))
         (subject (or (frame-workflow--find-subject name)
                      (frame-workflow-define-subject name
                        :make-frame
                        `(if (and (bound-and-true-p frame-purpose-mode)
                                  frame-workflow-use-frame-purpose-for-directory)
                             (frame-purpose-make-directory-frame ,dir)
                           (make-frame)))))
         (default-directory dir)
         (frame (frame-workflow-make-frame subject)))
    (when (functionp frame-workflow-directory-frame-action)
      (with-selected-frame frame
        (funcall frame-workflow-directory-frame-action)))))

(defun frame-workflow-magit-same-window ()
  "Run `magit-status' in the same window.

This function is intended as the value for
`frame-workflow-directory-frame-action'."
  (require 'magit)
  (let ((magit-display-buffer-function
         #'magit-display-buffer-same-window-except-diff-v1))
    (magit-status)))

(provide 'frame-workflow)
;;; frame-workflow.el ends here
