(defsystem "sfile"
  :depends-on ("hunchentoot" "cl-who")
  :author "seanptmaher@gmail.com"
  :license "MIT"
  :components
  ((:module src
            :serial t
            :components
            ((:file "sfile")))))
