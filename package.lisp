(defpackage #:carola
  (:use #:cl
        #:drakma)
  (:export :get-balances
           :make-currency
           :get-loan-orders
           :make-currency-pair
           :get-latest-ticker
           :get-order-book
           :get-trade-history))

