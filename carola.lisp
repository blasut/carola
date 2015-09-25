;;;; carola.lisp

(in-package #:carola)

;; Use the unexported symbol, used for signing the request.
(export (find-symbol "ALIST-TO-URL-ENCODED-STRING" 'drakma) 'drakma)

(defmacro str (&rest body)
  `(concatenate 'string ,@body))

(setf drakma:*header-stream* *standard-output*)

(defun make-request (action)
  (let* ((command-url "https://poloniex.com/public?command=")
         (ticker (str command-url action))
        (stream (drakma:http-request ticker :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (json:decode-json stream)))

(defun secret ()
  (osicat:environment-variable "api_secret"))

(defun api-key ()
  (osicat:environment-variable "api_key"))

(defun sign-request (params)
  (let* ((params (drakma:alist-to-url-encoded-string params :ascii 'drakma:url-encode))
         (hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array (secret)) :sha512)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array params))
    (ironclad:hmac-digest hmac)))

(defun generate-nonce ()
  (write-to-string (local-time:timestamp-to-unix (local-time:now))))

(defun make-post (command)
  (let* ((command-url "https://poloniex.com/tradingApi")
         (params (pairlis '("command" "nonce") (list command (generate-nonce))))
         (sign (ironclad:byte-array-to-hex-string (sign-request params)))
         (stream (drakma:http-request command-url
                                      :want-stream t
                                      :method :post
                                      :parameters params
                                      :additional-headers (pairlis '("Sign" "Key") (list sign (api-key))))))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (json:decode-json stream)))

(defun get-balances ()
  (make-post "returnBalances"))

(defclass currency ()
  ((name
    :initarg :name
    :initform (error "Must supply a value for :name")
    :reader name
    :documentation "The name of the currency.")
   (url-name
    :reader url-name
    :documentation "The url name for the currency.")))

(defun make-currency (name)
  (make-instance 'currency :name name))

(defmethod initialize-instance :after ((currency currency) &key)
  (setf (slot-value currency 'url-name) (string-upcase (name currency))))

(defmethod get-loan-orders ((currency currency))
  (with-slots (name) currency
    (make-request (str "returnLoanOrders&currency=" (url-name currency)))))


;; implement for a single currency:
; createLoanOffer
; cancelLoanOffer
; returnOpenLoanOffers
; returnActiveLoans
; toggleAutoRenew
; returnLoanOrders



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
    :reader api-name
    :documentation "The name returned from the API.")
   (url-name
    :reader url-name
    :documentation "The name used in the URL for requests."))
  (:documentation "This class describes a currency-pair for the exchange. A currency-pair always have a from and to currency, for example. BTC to XMR."))

(defun make-currency-pair (from to)
  (make-instance 'currency-pair :from from :to to))

(defmethod initialize-instance :after ((currency-pair currency-pair) &key)
  (let ((from (url-name (slot-value currency-pair 'from)))
        (to (url-name (slot-value currency-pair 'to))))
    (setf (slot-value currency-pair 'api-name) (str "+" from "-" to "+"))
    (setf (slot-value currency-pair 'url-name) (str from "_" to))))

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
