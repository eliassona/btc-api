(ns btc-api.local
  
  (:use [clojure.pprint])
  (:require [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [btc-api.core :refer :all]))
(defn cli [& args]
  (let [res (apply sh (concat ["bitcoin-cli" "-regtest"] args))]
    (if (= (:exit res) 0)
      (apply str (butlast (:out res)))
      (throw (IllegalStateException. (:err res))))))


(defmacro def-cli [cmd & args] `(defn ~cmd [~@args] (cli ~(str cmd) ~@args)))

#_(defmacro def-json-bx [cmd & args]
   `(defn ~cmd [~@args]
      (let [bx-fn# (partial bx ~(str cmd) "-f" "json")]
          (json/read-json (bx-fn# ~@args)))))



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


(comment
  (def n (BtcCore.))
  (.tx-of n (first (:tx (.block-of n 100))))
)