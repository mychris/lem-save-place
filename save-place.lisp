;;;; -*- mode: lisp; coding: utf-8 -*-
;;;; Save place package for lem
;;;;

;; TODOs:
;;  * Add a limit, otherwise the cache grows indefinetly
;;    might require to get away from a hash-table and to a list/vector
;;
;;  * Better handling of directory buffers? Maybe store the name the cursor is on?

(defpackage :save-place
  (:use :cl :lem)
  (:export
   ;; customization variables
   :*file*
   ;; mode
   :save-place-mode))

(in-package :save-place)

(setf (documentation *package* t)
      "A global minor mode for the lem editor.
When the SAVE-PLACE mode is enabled, it stores the current cursor position when a buffer
is killed and returns to this position, if the file the buffer is associated with is
loaded again.

The file to which the information is stored can be configured using the *FILE* variable.
It will be loaded automatically when needed.  When lem exits, the gathered information
will be stored automatically into the file.")

(defvar *file* #P"save-place.lisp-expr")

(defvar *save-place-table* nil)

(defstruct save-place
  (filename)
  (filetype)
  (position))

(defun save-place-serialize (entry)
  (list (save-place-filename entry)
        (save-place-filetype entry)
        (save-place-position entry)))

(defun save-place-deserialize (list)
  (make-save-place :filename (nth 0 list)
                   :filetype (nth 1 list)
                   :position (nth 2 list)))

(define-minor-mode save-place-mode
    (:name "save-place"
     :global t
     :description "A global minor mode which saves the current cursor position of a buffer
and jumps to it, if the file the buffer is associated with is loaded again."
     :disable-hook '%disable
     :enable-hook '%enable))

(defun load-from-file ()
  (setq *save-place-table* (if *save-place-table*
                               (clrhash *save-place-table*)
                               (make-hash-table :test #'equal)))
  (let ((full-path (if (uiop:relative-pathname-p *file*)
                       (uiop:merge-pathnames* *file* (lem-home))
                       *file*)))
    (when (uiop:file-exists-p full-path)
      (handler-case
          (with-open-file (input full-path
                                 :direction :input)
            (loop :for save-place-line :in (read input)
                  :do (let ((save-place-entry (save-place-deserialize save-place-line)))
                        (setf (gethash (save-place-filename save-place-entry) *save-place-table*)
                              save-place-entry))))
        (sb-int:simple-file-error (c)
          (message "save-place: ~a~&" c))))))

(defun maybe-load-from-file ()
  (unless *save-place-table*
    (load-from-file))
  (unless *save-place-table*
    (setq *save-place-table* (make-hash-table :test #'equal))))

(defun save-to-file ()
  (unless *save-place-table*
    (load-from-file))
  (let ((full-path (if (uiop:relative-pathname-p *file*)
                       (uiop:merge-pathnames* *file* (lem-home))
                       *file*)))
    (handler-case
        (with-open-file (output full-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write (loop :for entry :being :the :hash-value :in *save-place-table*
                       :collect (save-place-serialize entry))
                 :stream output)
          (write-line "" output)
          nil)
      (sb-int:simple-file-error (c)
        (message "save-place: ~a~&" c)))))

(defun apply-stored-place (&rest buffers)
  (dolist (buffer buffers)
    (alexandria:when-let* ((filename (buffer-filename buffer))
                           (entry (gethash filename *save-place-table*)))
      (move-to-position (buffer-point buffer) (save-place-position entry)))))

(defun register-place (&rest buffers)
  (dolist (buffer buffers)
    (alexandria:when-let* ((filename (buffer-filename buffer)))
      (setf (gethash filename *save-place-table*)
            (make-save-place :filename filename
                             :filetype :file
                             :position (position-at-point (buffer-point buffer)))))))

(defun find-file-fun (buffer)
  (maybe-load-from-file)
  (add-hook (variable-value 'kill-buffer-hook :buffer buffer) #'register-place)
  (apply-stored-place buffer))

(defun kill-buffer-fun (buffer)
  (maybe-load-from-file)
  (register-place buffer))

(defun %enable ()
  (add-hook *find-file-hook* #'find-file-fun)
  (dolist (buffer (buffer-list))
    (when (buffer-filename buffer)
      (add-hook (variable-value 'kill-buffer-hook :buffer buffer) #'kill-buffer-fun))))

;; EXIT-EDITOR disables all the modes
(defun %disable ()
  (remove-hook *find-file-hook* #'find-file-fun)
  (dolist (buffer (buffer-list))
    (register-place buffer)
    (remove-hook (variable-value 'kill-buffer-hook :buffer buffer) #'kill-buffer-fun))
  (save-to-file))
