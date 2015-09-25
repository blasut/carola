;;;; carola.asd

(asdf:defsystem #:carola
  :description "For accessing the Poloniex API."
  :author "Blasut"
  :license "See license document."
  :depends-on (#:drakma
               #:cl-json
               #:ironclad
               #:local-time
               #:osicat)
  :serial t
  :components ((:file "package")
               (:file "carola")))

