;;;; package.lisp

(defpackage #:quicklisp-client-uploader
  (:use #:cl)
  (:export #:publish-client
           #:upload-quicklisp-lisp)
  (:import-from #:zs3
                #:all-keys
                #:name
                #:etag
                #:file-etag)
  (:import-from #:ironclad
                #:digest-file
                #:byte-array-to-hex-string)
  (:shadowing-import-from #:ql-http
                          #:url
                          #:hostname
                          #:path))

