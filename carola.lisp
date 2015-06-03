;;;; carola.lisp

(in-package #:carola)

(ql:quickload 'drakma)
(ql:quickload 'cl-json)

(defmacro str (&rest body)
  `(concatenate 'string ,@body))

(setf drakma:*header-stream* *standard-output*)

(defun make-request (action)
  (let* ((command-url "https://poloniex.com/public?command=")
         (ticker (str command-url action))
        (stream (drakma:http-request ticker :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (json:decode-json stream)))

(defclass currency-pair ()
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
  (:documentation "This class describes a currency-pair for the exchange. A currency-pair always have a from and to currency, for example. BTC to XMR."))

(defmethod initialize-instance :after ((currency-pair currency-pair) &key)
  (let ((from (slot-value currency-pair 'from))
        (to (slot-value currency-pair 'to)))
    (setf (slot-value currency-pair 'api-name) (string-upcase (str "+" from "-" to "+")))
    (setf (slot-value currency-pair 'url-name) (string-upcase (str from "_" to)))))

(defgeneric get-latest-ticker (currency-pair))
(defgeneric get-trade-history (currency-pair))
(defgeneric get-order-book (currency-pair))


(defmethod get-latest-ticker ((currency-pair currency-pair))
  (with-slots (api-name) currency-pair
    (remove-if-not #'(lambda (x) (string= (string (first x)) api-name)) (make-request "returnTicker"))))

(defmethod get-order-book ((currency-pair currency-pair))
  (with-slots (url-name) currency-pair
    (make-request (str "returnOrderBook&currencyPair=" url-name "&depth=50"))))

(defmethod get-trade-history ((currency-pair currency-pair))
  (with-slots (url-name) currency-pair
    (make-request (str "returnTradeHistory&currencyPair=" url-name))))
