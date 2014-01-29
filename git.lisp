;;;; git.lisp

(in-package #:quicklisp-client-uploader)

(defun git-output (directory &rest args)
  (commando:with-posix-cwd directory
    (apply #'commando:run-output-lines "git" args)))

(defun check-clean-git (directory)
  (let ((lines (git-output directory "status" "-s")))
    (when lines
      (error "Directory ~A has dirty git status" directory))
    t))

(defun check-version-tag (directory version)
  (let ((lines (git-output directory "tag" "-l" (format nil "version-~A"
                                                        version))))
    (unless lines
      (error "No git tag for version-~A found in ~A" version directory))
    t))

