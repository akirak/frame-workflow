;;; frame-workflow-prototype.el --- Prorotypes for frame workflow -*- lexical-binding: t -*-

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

;; This library defines `frame-workflow-prototype-alist' custom variable and
;; operations on the variable. These are used in the main library of
;; `frame-workflow.el'.

;;; Code:

(require 'cl-lib)
(require 'seq)

;;;; The custom variable
(defcustom frame-workflow-prototype-alist nil
  "Alist of workflow prototypes.

Each item in this list must conform to (NAME . PLIST) where NAME is a symbol
which uniquely identifies the prototype and PLIST is a list of prototype
properties.

The following properties are accepted:

`:make-frame-function':
  A function (a symbol or a lambda expression) or an expression used to create
  a frame.

  - If it is a function, it must take the same arguments as `make-frame',
    i.e. an alist of frame parameters, and return a frame.

  - If it is an expression, it is evaluated using `eval'. The evaluated
    expression must return a frame.

  If you don't specify this property, `make-frame' is used as the default value.

`:init-form':
  An expression used to initialize a state after the frame is created.

  It may contain window/buffer operations such as creating windows and switching
  to a buffer, but its returned value is used as the initial state of the frame
  workflow. The workflow state can be retrieved and updated using
  `frame-workflow-get-state' and `frame-workflow-set-state' respectively,
  and it can be displayed in the modeline.

`:actions':
  A list of actions available in frames based on the prototype.

  Each item in this list is a list with 2/3 items, which is similar to a binding
  in Hydra:

  - The first item is a string representing a key to invoke the action.
    If this value is non-nil, you can use the key to invoke the action in
    `frame-workflow-action-map'.

  - The second item is a symbol to a function symbol, a lambda expression, or
    an S expression. If it is a symbol, it will be run as an interactive command
    when the action is triggered. If it is a lambda expression, it will be called.
    If it is an expression, it is evaluated.

  - The third item is an optional title to describe the action. It is displayed
    in prompt interfaces. If you omit this value, the action is somehow printed
    as a string.

  These actions are available in `frame-workflow-select-action' (all) and
  `frame-workflow-action-map' (only those with key properties).

`:state-formatter':
  A function to format the state of the workflow. It must take a state as an
  argument and return a string. The result is displayed in the modeline if the
  state is non-nil. Note that if the state is nil, the state isn't displayed in
  the modeline, so you don't have to consider nil as an argument.

  If this property is omitted, `prin1-to-string' is used as the default."
  :group 'frame-workflow
  :type '(alist
          :key-type
          (symbol :tag "Name")
          :value-type
          (plist :options
                 (((const :tag "Function to create a frame" :make-frame-function)
                   (sexp :tag "Function/sexp"))
                  ((const :tag "Init form" :init-form)
                   sexp)
                  ((const :tag "Actions" :actions)
                   ;; It can be configured like Hydra bindings
                   (repeat (list (string :tag "Key")
                                 (sexp :tag "Function/sexp")
                                 (string :tag "Label (optional)"))))
                  ((const :tag "State formatter" :state-formatter)
                   function)))))

;;;###autoload
(defun frame-workflow-prototype-customize-all ()
  "Customize the entire list of prototypes."
  (interactive)
  (customize-variable 'frame-workflow-prototype-alist))

;;;; Functions to generate a list of prototype names
(defun frame-workflow-prototype--configured-names ()
  "Get a list of prototype names configured in `frame-workflow-prototype-alist'."
  (mapcar 'car frame-workflow-prototype-alist))

(defun frame-workflow-prototype--active-names (&optional other)
  "Get a list of prototype names in the current frames.

If OTHER is non-nil, exclude the current frame."
  (cl-delete-duplicates
   (remq nil (mapcar (lambda (frm)
                       (frame-parameter frm 'workflow-prototype))
                     (if other
                         (delq (selected-frame) (frame-list))
                       (frame-list))))))

(defun frame-workflow-prototype--inactive-names ()
  "Get a list of prototype names that are not in the current frames but configured."
  (seq-difference (frame-workflow-prototype--configured-names)
                  (frame-workflow-prototype--active-names)))

;;;; Create a new prototype
(defun frame-workflow-prototype--new (name &optional initial)
  "Add a new prototype named NAME with INITIAL properties."
  (let ((cell (assq name frame-workflow-prototype-alist)))
    (unless cell
      (push (cons name initial) frame-workflow-prototype-alist))))

;;;; Look up properties of a prototype
(defun frame-workflow-prototype--properties (name &optional require)
  "Get the properties of a NAME as a plist.

If REQUIRE is non-nil and the prototype does not exist in
`frame-workflow-prototype-alist', this function throws an error."
  (let ((pair (assq name frame-workflow-prototype-alist)))
    (cond
     (pair (cdr pair))
     (require (error "Prototype %s does not exist" (symbol-name name))))))

(defun frame-workflow-prototype--lookup-property (key prototype)
  "Look up a property KEY or PROTOTYPE.

If the prototype does not exist, this function raises an error.

If the prototype exists but the property does not exist, this function returns
nil."
  (plist-get (frame-workflow-prototype--properties prototype t) key))

;;;; Update a particular property of a prototype
(defun frame-workflow-prototype--put-property (prototype key value
                                                         &optional allow-create)
  "Set a property of a prototype.

PROTOTYPE is the name of the prototype, which is a symbol.

KEY is the name of a property, and VALUE is the new value.

If the prototype does not exist but ALLOW-CREATE is non-nil, a new prototype is
created."
  (let ((plist (or (frame-workflow-prototype--properties prototype (not allow-create))
                   (let ((cell (cons prototype nil)))
                     (nconc frame-workflow-prototype-alist (cons cell nil))
                     (cdr cell)))))
    (plist-put plist key value)))

;;;; Persistence

;;;###autoload
(defun frame-workflow-prototype-save-all ()
  "Save the prototypes to the custom file."
  (interactive)
  (customize-save-variable 'frame-workflow-prototype-alist
                           frame-workflow-prototype-alist))

;;;###autoload
(defun frame-workflow-prototype-reload-all ()
  "Reload prototypes from the custom file."
  (interactive)
  (custom-load-symbol 'frame-workflow-prototype-alist))

;;;; Prompt function
(cl-defun frame-workflow-prototype--read-name (prompt candidates
                                                      &key require-match)
  "Get a prototype name from the user using `completing-read' interface.

PROMPT is a string, and CANDIDATES is a list of prototype names which should be
symbols. If REQUIRE-MATCH is non-nil, only an item in CANDIDATES can be returned.

This function returns a symbol."
  (let ((name (completing-read prompt candidates nil require-match)))
    (cl-etypecase name
      (symbol name)
      (string (intern name)))))

;;;; Creating a frame from a prototype
(defun frame-workflow-prototype--make-frame (name &optional params)
  "Make a frame for a given prototype.

This function uses a function configured as `:make-frame-function' property in
an item in the prototype alist to create a frame. If the property is not
configured, `make-frame' is used as the default

NAME is the prototype which is the car of an item in the prototype list.

PARAMS is passed as an argument to the function unless the property is a
non-lambda expression."
  (let ((func (frame-workflow-prototype--lookup-property :make-frame-function name)))
    (cl-typecase func
      (null (funcall 'make-frame params))
      (functionp (funcall func params))
      (otherwise (eval func)))))

(provide 'frame-workflow-prototype)
;;; frame-workflow-prototype.el ends here
