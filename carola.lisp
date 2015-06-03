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

;; A currency always have a from and to currency, for example. BTC to XMR. Can't be standalone.
(defclass currency ()
  ((from
    :initarg :from)
   (to
    :initarg :to)
   ticker-name
   api-name))

(defmethod initialize-instance :after ((currency currency) &key)
  (let ((from (slot-value currency 'from))
        (to (slot-value currency 'to)))
    (setf (slot-value currency 'ticker-name) (string-upcase (str "+" from "-" to "+")))))

(defmethod initialize-instance :after ((currency currency) &key)
  (let ((from (slot-value currency 'from))
        (to (slot-value currency 'to)))
    (setf (slot-value currency 'api-name) (string-upcase (str from "_" to)))))

;; Expects: Two strings in the format: XXX, XXX+
;;          Both strings should be 3 or more characters long.
;; Example: BTC, LTC
(defun get-latest-ticker-for-currency (from to)
  (let ((currency-name (string-upcase (str "+" from "-" to "+")))
        (ticker (get-latest-ticker)))
    (remove-if-not #'(lambda (x) (string= (string (first x)) currency-name)) ticker)))

(defun get-order-book-for-currency (from to)
  (let* ((currency-name (string-upcase (str from "_" to)))
         (ticker (str *command-url* "returnOrderBook&currencyPair=" currency-name "&depth=50"))
           (stream (drakma:http-request ticker :want-stream t)))
      (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
      (json:decode-json stream)))
