;;;; carola.asd

(asdf:defsystem #:carola
  :description "Describe carola here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "carola")))

