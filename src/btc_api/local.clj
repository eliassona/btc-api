(ns btc-api.local
  
 (:use [clojure.pprint])
 (:require [clojure.data.json :as json]
           [clojure.java.shell :refer [sh]]
           #_[btc-api.core :refer :all]))

(def bitcoin-cli "/Users/anderseliasson/src/bitcoin/src/bitcoin-cli")

(defn cli [& args]
  (let [res (apply sh (concat [bitcoin-cli] (map str args)))]
    (if (= (:exit res) 0)
      (apply str (butlast (:out res)))
      (throw (IllegalStateException. (:err res))))))


(defmacro def-cli-json [cmd & args] 
  `(defn ~cmd [~@args] 
     (let [res# (cli ~(str cmd) ~@args)]
       (try 
         (json/read-str res#)
         (catch Exception e#
           res#)))))

(defmacro def-cli [cmd & args] 
  `(defn ~cmd [~@args] 
     (cli ~(str cmd) ~@args)))

(def-cli getrawtransaction tx-id) 
(def-cli createrawtransaction in out)
(def-cli-json decoderawtransaction raw-tx)
(defn createmultisig [m pub-keys]
  (json/read-str (cli "createmultisig" (str m) (json/json-str pub-keys))))
(defn signrawtransactionwithkey [tx-in-hex private-keys]
  (json/read-str (cli "signrawtransactionwithkey" tx-in-hex (json/json-str private-keys))))

;blockchain
(def-cli getblockcount)
(def-cli getblockhash height)
(def-cli-json getblockheader block-hash)
(def-cli-json getblock block-hash)

(defn block-header-of [height-or-hash]
    (getblockheader 
      (if (number? height-or-hash) 
        (getblockhash height-or-hash) 
        height-or-hash)))

(defn block-of [height-or-hash]
         (getblock 
           (if (number? height-or-hash) 
             (getblockhash height-or-hash) 
             height-or-hash)))

(defn tx-of [tx-hash]
  (decoderawtransaction (getrawtransaction tx-hash)))



;wallet
(def-cli getnewaddress)
(defn sendtoaddress! [address btc] (cli "sendtoaddress" address (str btc)))
(def-cli-json getbalance)
(def-cli listwallets)
(defn addmultisigaddress! [m hex-or-address-pub-key] 
  (json/read-str (cli "addmultisigaddress" m (json/json-str hex-or-address-pub-key))))


;examples

(def key-1 {:private "5KffUB9YUsvoGjrcn76PjVnC61PcWLzws4QPfrT9RFNd85utCkZ"
            :public "04A97B658C114D77DC5F71736AB78FBE408CE632ED1478D7EAA106EEF67C55D58A91C6449DE4858FAF11721E85FE09EC850C6578432EB4BE9A69C76232AC593C3B"})
(def key-2 {:private "5KawqZHB1H6Af12ZhgTBXwQUY1jACgvGMywET7NF5bdYYzCxomY"
            :public "04019EF04A316792F0ECBE5AB1718C833C3964DEE3626CFABE19D97745DBCAA5198919081B456E8EEEA5898AFA0E36D5C17AB693A80D728721128ED8C5F38CDBA0"})
(def key-3 {:private "5KiDGG8sfmTNnzKDmm1MteWHV2TQQaUBbaEY3huVLwVz1i6i5be"
            :public "04A04F29F308160E6F945B33D943304B1B471ED8F9EACEEB5412C04E60A0FAB0376871D9D1108948B67CAFBC703E565A18F8351FB8558FD7C7482D7027EECD687C"})

(def multisig (createmultisig 2 (map :public [key-1 key-2 key-3])))

(def pay-to-address "tb1qxpqaddgec3thrkl2s5za7zv6gzy9d23whl26aw")
(comment
  (sendtoaddress! (multisig "address") (/ (getbalance)  10))
  (def tx-id "a88f4aaface0bfd90e9f1109db3449a67f090fa9051296ce22842babb8fb9ede")
  (tx-of tx-id)
  (def pay-tx (createrawtransaction (json/json-str [{:txid tx-id, "vout" 1}]) (json/json-str [{pay-to-address 0.00009}])))
  )
  

(comment
  (def n (BtcCore.))
  (.tx-of n (first (:tx (.block-of n 100))))
)