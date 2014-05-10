;;;; quicklisp-client-uploader.lisp

(in-package #:quicklisp-client-uploader)

;;; "quicklisp-client-uploader" goes here. Hacks and glory await!

(defvar *format* "1")

(defparameter *bucket* "beta.quicklisp.org")

(defparameter *paths-to-invalidate*
  '("/client/quicklisp.sexp"
    "/quickstart/version.txt"
    "/quickstart/quicklisp.tar"
    "/quickstart/asdf.lisp"
    "/quickstart/setup.lisp"))

(defun distributions-for-bucket (bucket)
  (loop for distribution in (ignore-errors (zs3:all-distributions))
        when (member bucket (zs3:cnames distribution) :test 'equalp)
        collect distribution))

(defun invalidate-paths (bucket paths)
  (loop for distribution in (distributions-for-bucket bucket)
        collect (zs3:invalidate-paths distribution paths)))

(defun gzipped-url (url)
  (concatenate 'string url ".gz"))

(defun gzipped-file (file)
  ;;; UGH.
  (truename (format nil "~A.gz" (namestring file))))

(defun flush-alpha-client-files ()
  (let ((bucket "alpha.quicklisp.org"))
    (flet ((del (key)
             (format *trace-output* "; Deleting ~A~%" key)
             (zs3:delete-object bucket (name key))))
      (map nil #'del (zs3:all-keys bucket :prefix "client/"))
      (map nil #'del (zs3:all-keys bucket :prefix "asdf/")))))

(defun existing-client-files (&key (bucket *bucket*))
  (let ((table (make-hash-table :test 'equal)))
    (labels ((key-url (key)
               (format nil "http://~A/~A" bucket (name key)))
             (save-key (key)
               (setf (gethash (etag key) table) (key-url key))))
      (map nil #'save-key (all-keys bucket :prefix "client/"))
      (map nil #'save-key (all-keys bucket :prefix "asdf/")))
    table))

(defun file-md5 (file)
  (byte-array-to-hex-string
   (digest-file :md5 file)))

(defun file-sha256 (file)
  (byte-array-to-hex-string
   (digest-file :sha256 file)))

(defun file-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun client-file-info-plist (url file table)
  (let* ((etag (file-etag file))
         (size (file-size file))
         (sha256 (file-sha256 file))
         (md5 (file-md5 file))
         (url (or (gethash etag table) url)))
    (list :url url
          :size size
          :md5 md5
          :sha256 sha256)))

(defun write-client-file-info-plist (key plist stream)
  (format stream " ~(~S~)~%" key)
  (destructuring-bind (&key url size md5 sha256)
      plist
    (format stream " (:url ~S~%" url)
    (format stream "  :size ~D~%" size)
    (format stream "  :md5 ~S~%" md5)
    (format stream "  :sha256~%  ~S)" sha256)))

(defun make-url (bucket path version file)
  (format nil "http://~A/~A/~@[~A/~]~A"
          bucket path version file))

(defun position-after-search (search-string target-string)
  (let ((start-position (search search-string target-string)))
    (when start-position
      (+ (length search-string) start-position))))

(defun guess-asdf-version-from-string (string)
  (let ((pos (position-after-search ";;; This is ASDF " string)))
    (when pos
      (let ((colon (position #\: string :start pos)))
        (when colon
          (subseq string pos colon))))))

(defun guess-asdf-version (asdf-file)
  (with-open-file (stream asdf-file)
    (loop for line = (read-line stream nil)
          while line
          do
          (let ((version (guess-asdf-version-from-string line)))
            (when version
              (return version)))
          finally (error "Could not guess version from ~A" asdf-file))))



(defun client-info-plist (&key
                            version
                            subscription-url
                            client-tar-file
                            setup-file
                            asdf-file
                            existing-client-files)
  (let* ((asdf-version (guess-asdf-version asdf-file))
         (asdf-url (make-url *bucket* "asdf" asdf-version "asdf.lisp"))
         (setup-url (make-url *bucket* "client" version "setup.lisp"))
         (client-tar-url (make-url *bucket* "client" version "quicklisp.tar"))
         (asdf-info (client-file-info-plist asdf-url asdf-file
                                            existing-client-files))
         (setup-info (client-file-info-plist setup-url setup-file
                                             existing-client-files))
         (client-tar-info (client-file-info-plist client-tar-url client-tar-file
                                                  existing-client-files))
         (canonical-client-info-url (make-url *bucket* "client" version
                                              "client-info.sexp")))
    (list :version version
          :client-info-format *format*
          :subscription-url subscription-url
          :canonical-client-info-url canonical-client-info-url
          :client-tar client-tar-info
          :setup setup-info
          :asdf asdf-info)))


(defun write-client-info-plist (plist stream)
  (destructuring-bind (&key version client-info-format
                            subscription-url canonical-client-info-url
                            client-tar setup asdf)
      plist
    (format stream "(:version ~S~%" version)
    (format stream " :client-info-format ~S~%" client-info-format)
    (terpri stream)
    (format stream " :subscription-url~%")
    (format stream " ~S~%" subscription-url)
    (format stream " :canonical-client-info-url~%")
    (format stream " ~S~%" canonical-client-info-url)
    (terpri stream)
    (write-client-file-info-plist :client-tar client-tar stream)
    (terpri stream)
    (terpri stream)
    (write-client-file-info-plist :setup setup stream)
    (terpri stream)
    (terpri stream)
    (write-client-file-info-plist :asdf asdf stream)
    (format stream ")~%")))

(defun first-line (file)
  (with-open-file (stream file)
    (read-line stream)))

(defun write-client-info-file (plist file)
  (with-open-file (stream file :direction :output)
    (write-client-info-plist plist stream)))

(defun already-published-p (file table)
  (not (not (gethash (file-etag file) table))))

(defun all-published-p (table &rest files)
  (every (lambda (file)
           (already-published-p file table))
         files))

(defun available-client-versions ()
  (let ((keys (zs3:all-keys *bucket* :prefix "client/"))
        (result '()))
    (dolist (key (coerce keys 'list) (sort result
                                           #'string>
                                           :key 'car))
      (ppcre:register-groups-bind (version)
          ("client/(.*?)/client-info.sexp$" (name key))
        (push (cons version (format nil "http://~A/~A" *bucket* (name key)))
              result)))))

(defun write-available-client-versions (file)
  (let ((versions (available-client-versions))
        (*print-pretty* nil))
    (with-open-file (stream file :direction :output)
      (format stream "(~{~S~^~% ~})~%"
              versions))
    (probe-file file)))

(defun publish-client-versions (versions-file)
  (put-to-s3-url versions-file
                 (make-url *bucket* "client" nil "quicklisp-versions.sexp")
                 :if-exists :overwrite
                 :content-type "text/plain"))

(defun publish-client (quicklisp-client)
  (check-clean-git quicklisp-client)
  (let ((client-version (first-line (merge-pathnames "quicklisp/version.txt"
                                                     quicklisp-client)))
        (subscription-url (make-url *bucket* "client" nil "quicklisp.sexp"))
        (client-tar-file (merge-pathnames "quicklisp.tar" quicklisp-client))
        (client-tgz-file (merge-pathnames "quicklisp.tar.gz" quicklisp-client))
        (setup-file (merge-pathnames "setup.lisp" quicklisp-client))
        (asdf-file (merge-pathnames "asdf.lisp" quicklisp-client))
        (existing-client-files (existing-client-files)))
    (check-version-tag quicklisp-client client-version)
    (commando:with-posix-cwd quicklisp-client
      (commando:run "make" "clean")
      (commando:run "make"))
    (when (all-published-p existing-client-files
                           client-tar-file setup-file asdf-file)
      (error "All files already published for tag ~A"
             client-version))
    (let ((plist (client-info-plist :version client-version
                                    :subscription-url subscription-url
                                    :client-tar-file client-tar-file
                                    :setup-file setup-file
                                    :asdf-file asdf-file
                                    :existing-client-files
                                    existing-client-files)))
      (commando:in-temporary-directory
        (write-client-info-file plist "client-info.sexp")
        (let ((client-info-url (make-url *bucket* "client" client-version
                                         "client-info.sexp")))
          (flet ((upload-client-file (key file type)
                   (unless (already-published-p file existing-client-files)
                     (let ((file-plist (getf plist key)))
                       (unless file-plist (error "Bogus key ~S" key))
                       (let ((url (getf file-plist :url)))
                         (unless url (error "Bogus file plist"))
                         (put-to-s3-url file url :content-type type)
                         (put-to-s3-url (gzipped-file file)
                                        (gzipped-url url)))))))
            (upload-client-file :client-tar client-tar-file
                                "binary/octet-stream")
            (upload-client-file :asdf asdf-file "text/plain")
            (upload-client-file :setup setup-file "text/plain")
            (put-to-s3-url "client-info.sexp" client-info-url
                           :content-type "text/plain")
            (put-to-s3-url "client-info.sexp" subscription-url
                           :if-exists :overwrite
                           :content-type "text/plain")
            (publish-client-versions
             (write-available-client-versions "quicklisp-versions.sexp"))
            (publish-legacy-files :version client-version
                                  :quicklisp-tar-file client-tar-file
                                  :quicklisp-tgz-file client-tgz-file
                                  :asdf-file asdf-file
                                  :setup-file setup-file)
            (invalidate-paths *bucket* *paths-to-invalidate*)
            t))))))

;;; Uploading quicklisp.lisp

(defun guess-quicklisp-bootstrap-version (quicklisp-file)
  (with-open-file (stream quicklisp-file)
    (loop for line = (read-line stream nil)
          while line do
          (let ((pos (position-after-search "qlqs-info:*version* " line)))
            (when pos
              (return (values (read-from-string line t t :start pos)))))
          finally (error "No version found in ~A" quicklisp-file))))

(defun upload-quicklisp-lisp (quicklisp-bootstrap)
  (check-clean-git quicklisp-bootstrap)
  (let* ((quicklisp-file (merge-pathnames "quicklisp.lisp" quicklisp-bootstrap))
         (version (guess-quicklisp-bootstrap-version quicklisp-file)))
    (check-version-tag quicklisp-bootstrap version)
    (let ((versioned-url (make-url *bucket* "quicklisp" version
                                   "quicklisp.lisp"))
          (top-url (format nil "http://~A/quicklisp.lisp" *bucket*)))
      (put-to-s3-url quicklisp-file versioned-url
                     :content-type "text/plain")
      (put-to-s3-url quicklisp-file top-url
                     :if-exists :overwrite
                     :content-type "text/plain"
                     :content-disposition "attachment")
      (list versioned-url top-url))))
