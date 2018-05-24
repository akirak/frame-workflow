;;; frame-workflow.el --- Per-frame workflow -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.12"))
;; URL: https://github.com/akirak/frame-workflow
;; Keywords: frames

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

;; This library lets you define per-frame workflow.

;;; Code:

;;;; Dependencies
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dash)
(require 'frame-workflow-prototype)

;;;; Custom variables

(defgroup frame-workflow nil
  "Framework for per-frame workflow."
  :prefix 'frame-workflow
  :group 'frames)

(defcustom frame-workflow-default-state-formatter 'prin1-to-string
  "Function used to format a workflow state when no formatter is specified."
  :group 'frame-workflow
  :type 'function)

(defcustom frame-workflow-mode-line-format "Frame[%s]"
  "Format string to represent the modeline.

When the frame has a workflow, this string is used the result of
`frame-workflow-mode-line' with \"%s\" replaced with its prototype name (if any)
and its state."
  :group 'frame-workflow
  :type 'string)

(defcustom frame-workflow-mode-line-state-format ":%s"
  "Format string to represent the state in the modeline.

When the state is non-nil, this string is appended to the prototype name with
\"%s\" replaced with the result of a formatter."
  :group 'frame-workflow
  :type 'string)

;;;; States

;;;###autoload
(defun frame-workflow-get-state ()
  "Get the workflow state of FRAME."
  (frame-parameter nil 'workflow-state))

;;;###autoload
(defun frame-workflow-set-state (new-state)
  "Set the workflow state of the frame.

NEW-STATE is the new value of the state, and FRAME is the target frame."
  (set-frame-parameter nil 'workflow-state new-state))

;;;;; Formatting a state

(defun frame-workflow--format-state (state &optional frame)
  "Format STATE using a state formatter of FRAME."
  (funcall (or (frame-parameter frame 'workflow-state-formatter)
               frame-workflow-default-state-formatter)
           state))

;;;; Actions

(defun frame-workflow--actions ()
  "Return a list of workflow actions for the current frame."
  (frame-parameter nil 'workflow-actions))

(defun frame-workflow--action-alist-by-name ()
  "Return an alist of workflow actions by title."
  (cl-loop for (_ action . r) in (frame-workflow--actions)
           for label = (car r)
           collect (cons (or label
                             (prin1-to-string action))
                         action)))

(defun frame-workflow--read-action (name)
  "Read a command or an expression for an action named NAME."
  (let ((choices '((?c . "Interactive command")
                   (?e . "Expression"))))
    (cl-case (read-char-choice (format "Define an action to \"%s\":\n%s\n" name
                                       (mapconcat
                                        (lambda (pair)
                                          (concat (char-to-string (car pair))
                                                  ": "
                                                  (cdr pair)))
                                        choices "\n"))
                               (mapcar #'car choices))
      (?c (read-command (format "Choose a command to \"%s\": " name)))
      (?e (read-minibuffer (format "Enter an expression that does \"%s\": " name))))))

(defun frame-workflow--push-action (binding)
  "Add an action BINDING to the frame parameter."
  (push binding (frame-parameter nil 'workflow-actions)))

(defun frame-workflow-add-new-action (label)
  "Define a new action named LABEL and store it in the frame parameter."
  (interactive "sName of the action to add: ")
  (let* ((action (frame-workflow--read-action label))
         ;; TODO: Prevent from duplicate bindings on the same key
         ;; (occupied-keys (remq nil (mapcar (lambda (l) (kbd (car l)))
         ;;                                  (frame-workflow--actions))))
         (key (read-key-sequence "Key used to access the action (C-g to omit): "))
         (binding (list (unless (equal key [7]) (key-description key))
                        action
                        label)))
    (frame-workflow--push-action binding)
    binding))

(defun frame-workflow-save-actions-to-prototype (&optional arg)
  "Save actions of the current frame to its prototype.

By default, this function confirms if the user wants to save all the prototypes
to the custom file. With ARG, it saves without confirmation."
  (interactive "P")
  (-when-let ((actions (frame-workflow--actions)))
    (let ((name (or (frame-workflow--prototype-name)
                    (intern (read-from-minibuffer "Enter the name for a new prototype: ")))))
      (frame-workflow-prototype--put-property name :actions actions)
      (when (or arg
                (yes-or-no-p "Save the prototypes to custom-file?"))
        (frame-workflow-prototype-save-all)))))

;;;###autoload
(defun frame-workflow-select-action ()
  "Select a workflow action interactively using `completing-read'."
  (interactive)
  (let* ((alist (frame-workflow--action-alist-by-name))
         (label (completing-read "Frame workflow action: "
                                 alist))
         (action (or (cdr (assoc label alist))
                     (nth 2 (frame-workflow-add-new-action label)))))
    (cl-etypecase action
      (symbol (call-interactively action))
      (function (funcall action))
      (listp (eval action)))))

(defun frame-workflow--action-to-command (action)
  "Execute an ACTION represented as either a command symbol or an expression."
  (cl-etypecase action
    (symbol action)
    (function `(lambda () (interactive) (funcall ,action)))
    (list `(lambda () (interactive) (eval ,action)))))

;;;###autoload
(defun frame-workflow-action-map ()
  "Run a workflow action by a key."
  (interactive)
  (let ((actions (frame-workflow--actions)))
    (cond
     (actions
      (let ((map (make-sparse-keymap)))
        (let (message-log-max)
          (message (concat
                    "Frame workflow action:\n"
                    (cl-loop for (key action . r) in actions
                             for label = (car r)
                             when (and key (not (string-empty-p key)))
                             do (define-key map (kbd key)
                                  (frame-workflow--action-to-command action))
                             concat (concat (propertize key
                                                        'face 'bold)
                                            ": "
                                            (or label (prin1-to-string action))
                                            "\n")))))
        (set-transient-map map t)))
     ((frame-workflow--has-workflow)
      (message "No actions"))
     (t
      ;; TODO: Associate a workflow
      (message "This frame has no workflow")))))

;;;; Prototypes
(defun frame-workflow--prototype-name (&optional frame)
  "Get the prototype name of FRAME."
  (frame-parameter frame 'workflow-prototype))

;;;;; Selecting a frame by prototype

(defcustom frame-workflow-select-frame-function
  'select-frame-set-input-focus
  "A function used to select a frame in frame-workflow.

It should be a function that takes a frame as the argument.
The following functions are suggested:

`select-frame-set-input-focus':
  Default. Works well with single-monitor settings, but not with multiple
  monitors, at least in EXWM.

`exwm-workspace-switch':
  Recommended for users of EXWM."
  :group 'frame-workflow
  :type 'function)

(defun frame-workflow--select-frame (frame)
  "Switch to FRAME using a peferred function."
  (funcall (or frame-workflow-select-frame-function
               #'select-frame-set-input-focus)
           frame))

(defun frame-workflow--find-frame (name)
  "Find a frame by a prototype NAME.

NAME can be either a symbol or a string, but the string is converted to a symbol."
  (let ((name-sym (cl-etypecase name
                    (symbol name)
                    (string (intern name)))))
    (cl-loop for frm in (frame-list)
             when (equal name-sym (frame-workflow--prototype-name frm))
             return frm)))

(defun frame-workflow--find-frames (name &optional other)
  "Find all frames of a given prototype.

Like `frame-workflow--find-frame', but returns all matching frames as a list of
frames.

NAME is a symbol or a string of the prototype.

If OTHER is non-nil, exclude the current frame."
  (let ((name-sym (cl-etypecase name
                    (symbol name)
                    (string (intern name)))))
    (cl-remove-if-not
     (lambda (frm) (equal name-sym (frame-workflow--prototype-name frm)))
     (if other
         (delq (selected-frame) (frame-list))
       (frame-list)))))

;;;###autoload
(defun frame-workflow-select-frame (name)
  "Select a frame by a prototype NAME."
  (interactive (list (frame-workflow-prototype--read-name
                      "Select a frame by prototype: "
                      (frame-workflow-prototype--active-names)
                      :require-match t)))
  (-when-let* ((frames (frame-workflow--find-frames name t))
               (frame-alist (mapcar (lambda (frm)
                                      (cons (frame-parameter frm 'name) frm))
                                    frames))
               (frm (if (cdr frames)
                        (cdr (assoc (completing-read
                                     (format "Select a frame of prototype %s: "
                                             (symbol-name name))
                                     frame-alist nil t)
                                    frame-alist))
                      (car frames))))
    (frame-workflow--select-frame frm)
    frm))

;;;;; Applying a prototype to a frame

(defvar frame-workflow-set-prototype-hook nil
  "Hook run after initializing a frame workflow.

This hook is run after applying a prototype, i.e. after frame parameters for
frame-workflow are set and the frame state is initialized. It is run after
\(a\) a frame is created by frame-workflow with a prototype and
\(b\) a prototype is applied to an existing frame with no prototype, but
not after it is re-applied to a frame with the same prototype.")

(defun frame-workflow--set-parameters (name plist)
  "Set frame parameters from prototype NAME with properties PLIST."
  (modify-frame-parameters nil
                           `((workflow-actions . ,(plist-get plist :actions))
                             (workflow-state-formatter . ,(plist-get plist :state-formatter))
                             (workflow-default-action . ,(plist-get plist :default-action))
                             (workflow-prototype . ,name))))

(defun frame-workflow--init-state (plist &optional frame)
  "Initialize the workflow state of a frame.

PLIST is a list of properties of the prototype, and FRAME is a frame on which
you have to set the state."
  (set-frame-parameter frame 'workflow-state
                       (with-selected-frame (or frame (selected-frame))
                         (eval (plist-get plist :init-form)))))

(defvar frame-workflow-duplicate-no-confirm nil
  "If non-nil, create multiple frames of the same prototype witout confirmation.")

(defun frame-workflow--confirm-new-frame (name)
  "Confirm if it is allowed to create a new frame of prototype NAME.

Return non-nil if there is no frame of prototype NAME or the user allows
duplicate frames of the prototype."
  (or frame-workflow-duplicate-no-confirm
      (not (frame-workflow--find-frame name))
      (yes-or-no-p (format "There is another frame of the same prototype %s. Are you sure creating another one?"
                           (symbol-name name)))))

;;;###autoload
(defun frame-workflow-apply-prototype (name &optional arg)
  "Apply a workflow prototype to the selected frame.

This function applies prototype named NAME to the current frame.
The behavior of this function depends on the current state of the frame:

+ If the frame does not have a workflow, choose a prototype and apply it.
  The workflow state is initialized based on the property of the prototype,
  and hooks configured as `frame-workflow-set-prototype-hook' are run.

  If there is another framae of the same prototype, the user is asked for
  confirmation. This confirmation step can be skipped by giving a prefix ARG.

+ If the frame already has a prototype, re-apply frame parameters of the
  prototype. In this operation, the workflow state is not re-initialized, and
  initialization hooks are not run.

+ If the frame has a workflow but no prototype, raise an error.

This function can also be used non-interactively. You can apply a prototype to
a new frame without a workflow by giving its prototype name as NAME.
This feature is used in frame-workflow-purpose.el."
  (interactive (list (or (frame-workflow--prototype-name)
                         (frame-workflow-prototype--read-name
                          "Apply prototype: "
                          (frame-workflow-prototype--configured-names)))
                     current-prefix-arg))
  (if (frame-workflow--prototype-name)
      (when (yes-or-no-p "This frame has a prototype. Reload parameters?")
        (frame-workflow--set-parameters name (frame-workflow-prototype--properties name t)))
    (if (frame-workflow--has-workflow)
        (error "Failed to apply a prototype: This frame already has workflow but not a prototype")
      (let ((plist (frame-workflow-prototype--properties name t)))
        (when (or arg (frame-workflow--confirm-new-frame name))
          (frame-workflow--set-parameters name plist)
          (frame-workflow--init-state plist)
          (run-hooks frame-workflow-set-prototype-hook))))))

;;;;; Creating a frame from a prototype

;;;###autoload
(defun frame-workflow-make-frame (name &optional allow-create &rest params)
  "Make a new frame of a given prototype.

NAME is the name of a prototype.

If a prefix argument ALLOW-CREATE is given and the prototype does not exist,
it creates a new one.

PARAMS is a list of frame parameters which takes the same form as that of
`make-frame'."
  (interactive (list (frame-workflow-prototype--read-name
                      "Select a prototype for new frame: "
                      (frame-workflow-prototype--configured-names))
                     current-prefix-arg))
  (when (frame-workflow--confirm-new-frame name)
    (let ((frame (condition-case nil
                     (frame-workflow-prototype--make-frame name params)
                   (error (if allow-create
                              (progn
                                (frame-workflow-prototype--new name)
                                (make-frame params))
                            (error "Prototype %s does not exist" (symbol-name name)))))))
      (with-selected-frame frame
        (frame-workflow-apply-prototype name t))
      frame)))

;;;###autoload
(defun frame-workflow-switch-frame (name)
  "Select or create a frame of prototype NAME.

If the prototype of NAME doesn't exist, a new empty prototype is added to the
list, and a new frame of the prototype is created."
  (interactive (list (frame-workflow-prototype--read-name
                      "Select/make a frame of a prototype: "
                      ;; Display prototypes of other live frames.
                      ;; If you need to display inactive prototypes, i.e.
                      ;; names of prototypes that are not associated with
                      ;; any live frames, you need to tweak this.
                      (frame-workflow-prototype--active-names t))))
  ;; If a frame exists, select-frame function returs the frame (non-nil).
  (or (frame-workflow-select-frame name)
      (frame-workflow-make-frame name t)))

;;;; Other utility functions
(defun frame-workflow--has-workflow (&optional frame)
  "Check if FRAME has any workflow parameter."
  (cl-intersection '(workflow-state workflow-prototype workflow-actions)
                   (mapcar #'car (frame-parameters frame))))

;;;; Modeline

;;;###autoload
(defun frame-workflow-mode-line ()
  "Build the modeline string on the frame workflow."
  (if (frame-workflow--has-workflow)
      (let ((name (frame-workflow--prototype-name))
            (state (frame-workflow-get-state)))
        (format frame-workflow-mode-line-format
                (concat (if name
                            (symbol-name name)
                          "Unnamed")
                        (if state
                            (format frame-workflow-mode-line-state-format
                                    (frame-workflow--format-state state))
                          ""))))
    nil))

;;;; Keymap
(defvar frame-workflow-map (make-sparse-keymap)
  "Keymap for commands in frame-workflow.")

(define-prefix-command 'frame-workflow-map)

(define-key frame-workflow-map "a" #'frame-workflow-apply-prototype)
(define-key frame-workflow-map "c" #'frame-workflow-make-frame)
(define-key frame-workflow-map "f" #'frame-workflow-select-frame)
(define-key frame-workflow-map "j" #'frame-workflow-switch-frame)
(define-key frame-workflow-map "m" #'frame-workflow-action-map)
(define-key frame-workflow-map "n" #'frame-workflow-add-new-action)
(define-key frame-workflow-map "C" #'frame-workflow-prototype-customize-all)
(define-key frame-workflow-map "S" #'frame-workflow-save-actions-to-prototype)

(provide 'frame-workflow)
;;; frame-workflow.el ends here
