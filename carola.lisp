;;;; carola.lisp

(in-package #:carola)

(ql:quickload 'drakma)

(defmacro str (&rest body)
  `(concatenate 'string ,@body))

(setf drakma:*header-stream* *standard-output*)

(defparameter *command-url* "https://poloniex.com/public?command=")

(defun get-latest-ticker ()
  (let* ((ticker (str *command-url* "returnTicker"))
        (stream (drakma:http-request ticker :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (json:decode-json stream)))



;;; "carola" goes here. Hacks and glory await!

