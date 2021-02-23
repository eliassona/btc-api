(ns btc-api.bchain
  (:require [bchain.core :as bchain]
            [btc-api.core :refer :all])
  )



(deftype BchainExchange []
  Exchange 
  (tickers [site] (bchain/rate-symbols-))
  (ticker [site symbol] ((bchain/ticker) symbol))
  )