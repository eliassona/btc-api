(ns btc-api.local
  
 (:use [clojure.pprint])
 (:require [clojure.data.json :as json]
           [clojure.java.shell :refer [sh]]
           [btc-api.core :refer :all]))
(defn cli [& args]
  (let [res (apply sh (concat ["/Users/anderseliasson/src/bitcoin/src/bitcoin-cli"] (map str args)))]
    (if (= (:exit res) 0)
      (apply str (butlast (:out res)))
      (throw (IllegalStateException. (:err res))))))


(defmacro def-cli [cmd & args] 
  `(defn ~cmd [~@args] 
     (let [res# (cli ~(str cmd) ~@args)]
       (try 
         (json/read-str res#)
         (catch Exception e#
           res#)))))

#_(defmacro def-json-cli [cmd & args]
   `(defn ~cmd [~@args]
      (let [cli-fn# (partial cli ~(str cmd) "-f" "json")]
          (json/read-json (cli-fn# ~@args)))))




(deftype BtcCore []
    Node
    (current-height [_] (read-string (cli "getblockcount")))
    (block-hash-of [_ height] (cli "getblockhash" (str height)))
    (block-header-of [node height-or-hash] 
      (json/read-json 
        (cli "getblockheader" 
             (if (number? height-or-hash) 
               (block-hash-of node height-or-hash) 
               height-or-hash))))
    (block-of [node height-or-hash]
      (json/read-json 
        (cli "getblock" 
             (if (number? height-or-hash) 
               (block-hash-of node height-or-hash) 
               height-or-hash))))
    (tx-of [node tx-hash]
      (json/read-json (cli "decoderawtransaction" (cli "getrawtransaction" tx-hash))))
)

;blockchain
(def-cli getblockcount)
(def-cli getblockhash height)


(defn createmultisig [m pub-keys]
  (json/read-str (cli "createmultisig" (str m) (json/json-str pub-keys))))

(def-cli getrawtransaction tx-id) 
(def-cli createrawtransaction in out)

;wallet
(def-cli getnewaddress)
(def-cli sendtoaddress address btc)
(def-cli getbalance)
(def-cli listwallets)


(def key-1 {:private "5KffUB9YUsvoGjrcn76PjVnC61PcWLzws4QPfrT9RFNd85utCkZ"
            :public "04A97B658C114D77DC5F71736AB78FBE408CE632ED1478D7EAA106EEF67C55D58A91C6449DE4858FAF11721E85FE09EC850C6578432EB4BE9A69C76232AC593C3B"})
(def key-2 {:private "5KawqZHB1H6Af12ZhgTBXwQUY1jACgvGMywET7NF5bdYYzCxomY"
            :public "04019EF04A316792F0ECBE5AB1718C833C3964DEE3626CFABE19D97745DBCAA5198919081B456E8EEEA5898AFA0E36D5C17AB693A80D728721128ED8C5F38CDBA0"})
(def key-3 {:private "5KiDGG8sfmTNnzKDmm1MteWHV2TQQaUBbaEY3huVLwVz1i6i5be"
            :public "04A04F29F308160E6F945B33D943304B1B471ED8F9EACEEB5412C04E60A0FAB0376871D9D1108948B67CAFBC703E565A18F8351FB8558FD7C7482D7027EECD687C"})

(def multisig (createmultisig 2 (map :public [key-1 key-2 key-3])))

(comment
  (sendtoaddress (multisig "address") (/ (getbalance)  10))
  (def tx-id "a88f4aaface0bfd90e9f1109db3449a67f090fa9051296ce22842babb8fb9ede")
  )
  

(comment
  (def n (BtcCore.))
  (.tx-of n (first (:tx (.block-of n 100))))
)