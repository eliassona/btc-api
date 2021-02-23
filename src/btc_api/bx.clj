(ns btc-api.bx
  (:require 
    [clojure.java.shell :refer [sh]]
    [clojure.data.json :as json]
    [btc-api.core :refer :all]))

(defn bx [& args]
  (let [res (apply sh (conj args "bx"))]
    (if (= (:exit res) 0)
      (apply str (butlast (:out res)))
      (throw (IllegalStateException. (:err res))))))


(defmacro def-bx [cmd & args] `(defn ~cmd [~@args] (bx ~(str cmd) ~@args)))

(defmacro def-json-bx [cmd & args]
  `(defn ~cmd [~@args]
     (let [bx-fn# (partial bx ~(str cmd) "-f" "json")]
         (json/read-json (bx-fn# ~@args)))))


(defn create-json-fn [name]
  (let [bx-fn (partial bx name "-f" "json")]
    (fn [& args]
      (json/read-json (apply bx-fn args)))))

(defn fetch-height [] (read-string (bx "fetch-height")))

(defn fetch-header [height-or-hash]
  (let [the-fn (create-json-fn "fetch-header")] 
    (if (number? height-or-hash)
      (the-fn "-t" (str height-or-hash))
      (the-fn "-s" height-or-hash))))


(def-bx sha160 base16-value)
(def-bx sha256 base16-value)
(def-bx sha512 base16-value)
(def-bx ripemd160 base16-value)
(def-bx base16-decode base-16-value)
(def-bx base16-encode data)
(def-bx base58-decode base-58-value)
(def-bx base58-encode base-16-value)
(def-bx base64-decode base-64-value)
(def-bx base64-encode data)
(def-json-bx base58check-decode base-58-check-value)
(def-bx base58check-encode base-16-value)

(deftype BxNode []
  Node
  (current-height [_] (fetch-height))
  (block-hash-of [node height])

  (block-header-of [_ height-of-index] (fetch-header height-of-index))
  (block-of [node height-or-hash])
  (tx-of [node tx-hash])
  )

(def-bx script-decode base-16-script) 
(def-bx script-encode script) 
(def-bx script-to-address script)
(def-json-bx tx-decode base-16-tx)
(defn tx-encode [tx-hash index payment-address sats]
  (let [format-fn (partial format "%s:%s")]
    (bx "tx-encode" "-i" (format-fn tx-hash index) "-o" (format-fn payment-address sats)))) 
   
(def-bx input-sign ec-private-key contract base-16-tx)
(def-bx input-set endorsement-script base-16-tx)    
(defn input-validate [ec-public-key contract endorsement base-16-tx]
  (= 
    (bx "input-validate" ec-public-key contract endorsement base-16-tx)
    "The endorsement is valid."))



(deftype BxCrypto []
  Crypto
  (sha160-of [_ base16-value] (sha160 base16-value))
  (sha256-of [_ base16-value] (sha256 base16-value))
  (sha512-of [_ base16-value] (sha512 base16-value))
  (ripemd160-of [_ base16-value] (ripemd160 base16-value))
  (base16-decode-of [_ base16-value] (base16-decode base16-value))
  (base16-encode-of [_ data] (base16-encode data))
  (base58-decode-of [_ base58-value] (base58-decode base58-value))
  (base58-encode-of [_ data] (base58-encode data))
  (base64-decode-of [_ base64-value] (base64-decode base64-value))
  (base64-encode-of [_ data] (base64-encode data))
  (script-decode-of [_ base-16-script] (script-decode base-16-script)) 
  (script-encode-of [_ script] (script-encode script)) 
  (script-to-address-of [_ script] (script-to-address script))
  (tx-decode-of [_ base-16-tx] (tx-decode base-16-tx))
  (tx-encode-of [_ tx-hash index payment-address sats] (tx-encode tx-hash index payment-address sats))
  (input-sign-of [_ ec-private-key contract base-16-tx] (input-sign ec-private-key contract base-16-tx))
  (input-set-of [_ endorsement-script base-16-tx] (input-set endorsement-script base-16-tx))    
  (input-validate-of [_ ec-public-key contract endorsement base-16-tx] (input-validate ec-public-key contract endorsement base-16-tx))
  )
  






