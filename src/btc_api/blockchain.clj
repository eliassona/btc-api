(ns btc-api.blockchain
  (:require [bchain.core :refer [getblockcount]]
            [btc-api.core :refer [Node Crypto]]))



(deftype BlockchainNode []
  Node
  (current-height [_] (getblockcount))
  )