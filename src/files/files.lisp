(in-package :sfile)

(defparameter *files* '())

(defmacro define-file (title filepath date desc)
  `(push (make-instance 'file :title ,title :desc ,desc :filepath ,filepath :date ,date) *files*))

(define-file "The grasshopper hops once in a blue moon" #p"testing.png" '(2020 13 13)
                "This is just a testfile, I'm really not sure what that file is")
