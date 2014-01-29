;;;; s3.lisp

(in-package #:quicklisp-client-uploader)

(defun s3-components (url)
  (setf url (url url))
  (values (hostname url)
          (string-left-trim "/" (path url))))

(defun s3-object-exists (bucket key)
  (let ((http-status (nth-value 1 (zs3:head :bucket bucket :key key))))
    (<= 200 http-status 299)))

(defun put-to-s3-url (file url &key (if-exists :error)
                      (content-type "binary/octet-stream")
                      content-disposition)
  (multiple-value-bind (bucket key)
      (s3-components url)
    (flet ((upload ()
             (zs3:put-file file bucket key
                      :public t
                      :content-type content-type
                      :content-disposition content-disposition)
             t))
      (ecase if-exists
        ((:overwrite)
         (upload))
        ((nil :error)
         (let ((exists (s3-object-exists bucket key)))
           (cond (exists
                  (case if-exists
                    (:error
                     (restart-case
                         (error "Object already exists at ~A" url)
                       (overwrite ()
                         :report "Overwrite it"
                         (upload))))
                    ((nil) nil)))
                 (t
                  (upload)))))))))
