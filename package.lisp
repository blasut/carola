(defpackage #:carola
  (:use #:cl
        #:drakma)
  (:export :make-currency
           :make-currency-pair
           :get-all-order-books
           :get-balances
           :get-complete-balances
           :get-deposit-addresses
           :get-deposit-and-withdrawals
           :get-all-open-orders
           :get-all-trade-histories
           :cancel-order
           :move-order
           :get-available-account-balances
           :get-tradable-balances
           :get-margin-account-summary
           :get-margin-positions
           :cancel-loan-order
           :get-open-loan-orders
           :get-active-loans
           :toggle-auto-renew
           :get-loan-orders
           :withdraw
           :transfer
           :create-loan-offer
           :generate-new-address
           :get-latest-ticker
           :get-order-book
           :get-trade-history
           :get-open-orders
           :buy
           :sell
           :margin-buy
           :margin-sell
           :get-margin-position
           :close-margin-position))
