;;;; legacy.lisp

(in-package #:quicklisp-client-uploader)

;;; The legacy file layout is:
;;;
;;;   quickstart/version.txt has an integer like "YYYYMMDDXX" in it,
;;;   e.g. 2014010100
;;;
;;;   quickstart/quicklisp.tar has the client for that version -
;;;   should be kept updated in case people have old quicklisp.lisps
;;;
;;;   quickstart/quicklisp-<version>.tgz has the client for that version
;;;
;;;   quickstart/setup.lisp and quickstart/asdf.lisp are downloaded
;;;   directly into <quicklisp>/
;;;
;;; So this just blorts the latest thing into place.

(defun cook-raw-version (raw-version)
  "Take a version string like \"2014-01-28\" and return a cooked
  version integer like 2014012800."
  (let ((cooked (make-string 10 :initial-element #\0))
        (digits (remove-if-not #'digit-char-p raw-version)))
    (replace cooked digits)
    (parse-integer cooked)))

(defun make-legacy-url (file)
  (make-url *bucket* "quickstart" nil file))

(defun put-legacy-file (file url &key binary overwrite)
  (let ((if-exists (if overwrite :overwrite :error))
        (content-type (if binary "binary/octet-stream" "text/plain")))
    (put-to-s3-url file url
                   :if-exists if-exists
                   :content-type content-type)))

(defun publish-legacy-files (&key version
                               quicklisp-tar-file
                               quicklisp-tgz-file
                               asdf-file
                               setup-file)
  (let* ((cooked-version (cook-raw-version version))
         (tar-url (make-legacy-url "quicklisp.tar"))
         (tgz-url (make-legacy-url (format nil "quicklisp-~A.tgz"
                                           cooked-version)))
         (setup-url (make-legacy-url "setup.lisp"))
         (asdf-url (make-legacy-url "asdf.lisp"))
         (version-url (make-legacy-url "version.txt")))
    (commando:in-temporary-directory
      (with-open-file (stream "version.txt" :direction :output)
        (format stream "~A~%" cooked-version))
      (put-legacy-file quicklisp-tgz-file tgz-url)
      (put-legacy-file quicklisp-tar-file tar-url :overwrite t)
      (put-legacy-file setup-file setup-url :overwrite t)
      (put-legacy-file asdf-file asdf-url :overwrite t)
      (put-legacy-file "version.txt" version-url :overwrite t))))
