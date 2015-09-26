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
    (format *standard-output* "~S" params)
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array params))
    (ironclad:hmac-digest hmac)))

(defun generate-nonce ()
  (write-to-string (local-time:timestamp-to-unix (local-time:now))))

(defun construct-params (params)
  (loop for param in params collect (cons (first param) (second param))))

(defun make-post (command &optional (params '()))
  (let* ((command-url "https://poloniex.com/tradingApi")
         (defaults (list (list "command" command) (list "nonce" (generate-nonce))))
         (params (construct-params (append params defaults)))
         (sign (ironclad:byte-array-to-hex-string (sign-request params)))
         (stream (drakma:http-request command-url
                                      :want-stream t
                                      :method :post
                                      :parameters params
                                      :additional-headers (pairlis '("Sign" "Key") (list sign (api-key))))))
    (format *standard-output* "~S" params)
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (json:decode-json stream)))

(defun get-all-order-books ()
  :documentation "Returns the order book for all markets."
  (make-request (str "returnOrderBook&depth=50")))

(defun get-balances ()
  :documentation "Returns all of your available balances."
  (make-post "returnBalances"))

(defun get-complete-balances ()
  :documentation "Returns all of your balances, including available balance, balance on orders, and the estimated BTC value of your balance."
  (make-post "returnCompleteBalances"))

(defun get-deposit-addresses ()
  :documentation "Returns all of your deposit addresses."
  (make-post "returnDepositAddresses"))

(defun get-deposit-and-withdrawals (&key start end)
  :documentation "Returns your deposit and withdrawal history within a range, both of which should be given as UNIX timestamps."
  (make-post "returnDepositsWithdrawals" (list (list "start" start)
                                               (list "end" end))))

(defun get-all-open-orders ()
  :documentation "Returns your open orders for all markets."
  (make-post "returnOpenOrders" '(("currencyPair" "all"))))

(defun get-all-trade-histories (&optional start end)
  :documentation "Returns your trade history for all markets. You may optionally specify a range via 'start' or 'end' given in UNIX timestamp format."
  (make-post "returnTradeHistory" '(("currencyPair" "all"))))

(defun cancel-order (order-number)
  :documentation "Cancels an order you have placed in a given market."
  (make-post "cancelOrder" (list (list "orderNumber" order-number))))

(defun move-order (order-number new-rate &optional amount)
  :documentation "Cancels an order and places a new one of the same type in a single atomic transaction, meaning either both operations will succeed or both will fail. You may optionally specify 'amount' if you wish to change the amount of the new order."
  (make-post "moveOrder" (list (list "orderNumber" order-number)
                               (list "newRate" new-rate))))

(defun get-available-account-balances (&optional account)
  :documentation "Returns your balances sorted by account. You may optionally specify the 'account' parameter if you wish to fetch only the balances of one account. Please note that balances in your margin account may not be accessible if you have any open margin positions or orders."
  (make-post "returnAvailableAccountBalances"))

(defun get-tradable-balances ()
  :documentation "Returns your current tradable balances for each currency in each market for which margin trading is enabled. Please note that these balances may vary continually with market conditions."
  (make-post "returnTradableBalances"))

(defun get-margin-account-summary ()
  :documentation "Returns a summary of your entire margin account. This is the same information you will find in the Margin Account section of the Margin Trading page, under the Markets list."
  (make-post "returnMarginAccountSummary"))

(defun get-margin-positions ()
  :documentation "Returns information about your margin position in all markets. If you have no margin position in a market, 'type' will be set to 'none'. 'liquidationPrice' is an estimate, and does not necessarily represent the price at which an actual forced liquidation will occur. If you have no liquidation price, the value will be -1."
  (make-post "getMarginPosition" '(("currencyPair" "all"))))

(defun cancel-loan-order (loan-number)
  :documentation "Cancels a loan offer."
  (make-post "cancelLoanOffer" (list (list "orderNumber" loan-number))))

(defun get-open-loan-orders ()
  :documentation "Returns your open loan offers for each currency."
  (make-post "returnOpenLoanOffers"))

(defun get-active-loans ()
  :documentation "Returns your active loans for each currency."
  (make-post "returnActiveLoans"))

(defun toggle-auto-renew (loan-number)
  :documentation "Toggles the autoRenew setting on an active loan."
  (make-post "toggleAutoRenew" (list (list "orderNumber" loan-number))))

(defun make-url-name (name)
  (string-upcase name))

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
  (setf (slot-value currency 'url-name) (make-url-name (name currency))))

(defmethod get-loan-orders ((currency currency))
  :documentation "Returns the list of loan offers and demands for a given currency."
  (with-slots (name) currency
    (make-request (str "returnLoanOrders&currency=" (url-name currency)))))

;; TODO: Implement different withdraw method for XMR only. 
(defmethod withdraw ((currency currency))
  :documentation "Immediately places a withdrawal for a given currency, with no email confirmation. In order to use this method, the withdrawal privilege must be enabled for your API key. For XMR withdrawals, you may optionally specify 'paymentId'.")

(defmethod transfer ((currency currency) &key amount from-account to-account)
  :documentation "Transfers funds from one account to another (e.g. from your exchange account to your margin account)."
  (make-post "transferBalance" (list (list "currency" (slot-value currency 'name))
                                     (list "amount" amount)
                                     (list "fromAccount" from-account)
                                     (list "toAccount" to-account))))

(defmethod create-loan-offer ((currency currency) &key amount duration auto-renew lending-rate)
  :documentation "Creates a loan offer for a given currency.")

(defmethod generate-new-address ((currency currency))
  :documentation "Generates a new deposit address for the currency. Addresses for some currencies do not generate immediately. All currencies added in the future will return addresses immediately. The ones that currently don't are being changed over to the new system."
  (make-post "generateNewAddress" (list (list "currency" (slot-value currency 'name)))))

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

(defun make-currency-pair (&key from to)
  (make-instance 'currency-pair :from from :to to))

(defmethod initialize-instance :after ((currency-pair currency-pair) &key)
  (let ((from (make-url-name (slot-value currency-pair 'from)))
        (to   (make-url-name (slot-value currency-pair 'to))))
    (setf (slot-value currency-pair 'api-name) (str "+" from "-" to "+"))
    (setf (slot-value currency-pair 'url-name) (str from "_" to))))

(defmethod get-latest-ticker ((currency-pair currency-pair))
  :documentation "Returns the ticker for the currency pair."
  (with-slots (api-name) currency-pair
    (remove-if-not #'(lambda (x) (string= (string (first x)) api-name)) (make-request "returnTicker"))))

(defmethod get-order-book ((currency-pair currency-pair))
  :documentation "Returns the order book for a given market."
  (with-slots (url-name) currency-pair
    (make-request (str "returnOrderBook&currencyPair=" url-name "&depth=50"))))

(defmethod get-trade-history ((currency-pair currency-pair) &optional start end)
  :documentation "Returns the past 200 trades for a given market, or all of the trades between a range specified in UNIX timestamps by the 'start' and 'end' parameters"
  (with-slots (url-name) currency-pair
    (make-request (str "returnTradeHistory&currencyPair=" url-name))))

(defmethod get-open-orders ((currency-pair currency-pair))
  :documentation "Returns your open orders for a given market."
  (make-post "returnOpenOrders" (list (list "currencyPair" (slot-value currency-pair 'url-name)))))

(defmethod buy ((currency-pair currency-pair) &key rate amount)
  :documentation "Places a buy order in a given market")

(defmethod sell ((currency-pair currency-pair) &key rate amount)
  :documentation "Places a sell order in a given market.")

(defmethod margin-buy ((currency-pair currency-pair) &key rate amount &optional lending-rate)
  :documentation "Places a margin buy order in a given market. Required  parameters are  'rate', and 'amount'. You may optionally specify a maximum lending rate using the 'lending-rate'. If successful, the method will return the order number and any trades immediately resulting from your order.")

(defmethod margin-sell ((currency-pair currency-pair) &key rate amount)
  :documentation "Places a margin buy order in a given market. Required  parameters are  'rate', and 'amount'. If successful, the method will return the order number and any trades immediately resulting from your order.")

(defmethod get-margin-position ((currency-pair currency-pair))
  :documentation "Returns information about your margin position in the market. If you have no margin position in the market, 'type' will be set to 'none'. 'liquidationPrice' is an estimate, and does not necessarily represent the price at which an actual forced liquidation will occur. If you have no liquidation price, the value will be -1.")

(defmethod close-margin-position ((currency-pair currency-pair))
  :documentation "Closes your margin position in a given market. This call will also return success if you do not have an open position in the specified market.")



