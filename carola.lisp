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

;; Expects: Two strings in the format: XXX, XXX+
;;          Both strings should be 3 or more characters long.
;; Example: BTC, LTC
(defun get-latest-ticker-for-currency (from to)
  (let ((ticker-name (string-upcase (str "+" from "-" to "+")))
        (ticker (get-latest-ticker)))
    (remove-if-not #'(lambda (x) (string= (string (first x)) ticker-name)) ticker)))




;;; "carola" goes here. Hacks and glory await!

