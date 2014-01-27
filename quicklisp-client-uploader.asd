;;;; quicklisp-client-uploader.asd

(asdf:defsystem #:quicklisp-client-uploader
  :serial t
  :description "Upload quicklisp-client pieces to S3."
  :author "Zach Beane <zach@quicklisp.org>"
  :license "BSD"
  :depends-on (#:zs3
               #:commando
               #:ironclad
               #:cl-ppcre)
  :components ((:file "package")
               (:file "s3")
               (:file "quicklisp-client-uploader")))
