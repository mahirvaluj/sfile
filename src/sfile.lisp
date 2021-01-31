(defpackage :sfile
  (:use :cl-user :cl)
  (:export))
(in-package :sfile)

;; dates are lists of length 3 with (year month date) of integers
(defun compare-dates (a b)
  (cond ((< (car a) (car b)) t)
        ((and (= (car a) (car b)) (< (cadr a) (cadr b))) t)
        ((and (= (car a) (car b)) (= (cadr a) (cadr b)) (< (caddr a) (caddr b))) t)
        (t nil)))

(defvar *acceptor*)
(defvar *files*)

(defclass file ()
  ((title :accessor title :initarg :title)
   (desc :accessor desc :initarg :desc)
   (date :accessor date :initarg :date)
   (filepath :accessor filepath :initarg :filepath)
   (filedir :accessor filedir :initarg :filedir)))

(defun load-eps (filepath)
  (unless (probe-file filepath)
    (error "could not find file path"))
  (let ((file (find-if (lambda (s) (string= (file-namestring s) "files.lisp")) (uiop:directory-files filepath))))
    (load file)
    (mapc (lambda (e)
            (setf (filedir e) filepath)
            (push (hunchentoot:create-static-file-dispatcher-and-handler
                   (concatenate 'string "/files/" (file-namestring (filepath e)))
                   (merge-pathnames (filepath e) filepath))
                  hunchentoot:*dispatch-table*))
          *files*)))

(defun run-server (port &optional (filepath "./files/"))
  (load-eps filepath)
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *acceptor*))

(defmethod file-to-html ((f file))
  (cl-who:with-html-output-to-string (s)
    (:p
     (:h3 (write-string (title f) s))
     (:h5 (write-string (format nil "~A" (date f)) s))
     (:div :class "description" (write-string (desc f) s))
     (:a :href (concatenate 'string "/files/" (file-namestring (filepath f))) "Download here"))))

(defun standard-page (title body)
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head
      (:link :href "/style/style.css" :rel "stylesheet")
      (:link :rel "preconnected" :href "https://fonts.gstatic.com")
      (:link :href "https://fonts.googleapis.com/css" :rel "stylesheet"))
     (:body
      (:h1 (write-string title s))
      (write-string body s)))))

(hunchentoot:define-easy-handler (home :uri "/" :acceptor-names t) ()
  (standard-page
   "Sean's filedump"
   (cl-who:with-html-output-to-string (s)
     (loop for i in *files* collect (cl-who:fmt (file-to-html i))))))

(push (hunchentoot:create-static-file-dispatcher-and-handler "/style/style.css" #p"www/style.css")
      hunchentoot:*dispatch-table*)
