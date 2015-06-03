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

(defclass currency ()
  ((from
    :initarg :from
    :initform (error "Must supply a value for :from")
    :documentation "The currency to count from.")
   (to
    :initarg :to
    :initform (error "Must supply a value for :to")
    :documentation "The currency to count to.")
   (api-name
    :accessor api-name
    :documentation "The name returned from the API.")
   (url-name
    :accessor url-name
    :documentation "The name used in the URL for requests."))
  (:documentation "This class describes a currency for the exchange. A currency always have a from and to currency, for example. BTC to XMR."))

(defmethod initialize-instance :after ((currency currency) &key)
  (let ((from (slot-value currency 'from))
        (to (slot-value currency 'to)))
    (setf (slot-value currency 'api-name) (string-upcase (str "+" from "-" to "+")))
    (setf (slot-value currency 'url-name) (string-upcase (str from "_" to)))))

(defmethod get-latest-ticker ((currency currency))
  (with-slots (api-name) currency
    (remove-if-not #'(lambda (x) (string= (string (first x)) api-name)) (make-request "returnTicker"))))

(defmethod get-order-book ((currency currency))
  (with-slots (url-name) currency
    (make-request (str "returnOrderBook&currencyPair=" url-name "&depth=50"))))
