;; Init file for development

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))

(require 'frame-workflow)

(frame-workflow-define-subject "home"
  :observer-class frame-workflow-observer)

(frame-workflow-define-subject "emacs-lisp")

;; TODO: Configure a modeline

(prin1 frame-workflow--subject-list)
