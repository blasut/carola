;;;; carola.lisp

(in-package #:carola)

(ql:quickload 'drakma)
(ql:quickload 'cl-json)

(defmacro str (&rest body)
  `(concatenate 'string ,@body))

(setf drakma:*header-stream* *standard-output*)

(defparameter *command-url* "https://poloniex.com/public?command=")

(defun make-request (action)
  (let* ((ticker (str *command-url* action))
        (stream (drakma:http-request ticker :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (json:decode-json stream)))

;; A currency always have a from and to currency, for example. BTC to XMR. Can't be standalone.
(defclass currency ()
  ((from
    :initarg :from)
   (to
    :initarg :to)
   (api-name
    :accessor api-name
    :documentation "The name returned from the API.")
   (url-name
    :accessor url-name
    :documentation "The name used in the URL for requests.")))

(defmethod initialize-instance :after ((currency currency) &key)
  (let ((from (slot-value currency 'from))
        (to (slot-value currency 'to)))
    (setf (slot-value currency 'api-name) (string-upcase (str "+" from "-" to "+")))
    (setf (slot-value currency 'url-name) (string-upcase (str from "_" to)))))

(defmethod get-latest-ticker ((currency currency))
  (with-slots (api-name) currency
    (remove-if-not #'(lambda (x) (string= (string (first x)) api-name)) (make-request "returnTicker"))))

(defun get-order-book-for-currency (from to)
  (let* ((currency-name (string-upcase (str from "_" to)))
         (ticker (str *command-url* "returnOrderBook&currencyPair=" currency-name "&depth=50"))
           (stream (drakma:http-request ticker :want-stream t)))
      (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
      (json:decode-json stream)))
